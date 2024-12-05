use advent_of_code::{Result, NameTable, Name, NameSet, NameMap};

struct Graph(NameMap<NameSet>);

type EdgeSet = std::collections::HashSet<(Name, Name)>;

enum AddNeighbors {
    NextQueue(Vec<Name>),
    Path(Vec<Name>),
}

fn compute_half_path(
    path: &mut Vec<Name>, mut node: Name, succ: &NameMap<Name>
) {
    loop {
        let &next = succ.get(&node).unwrap();
        if next == node {
            break;
        }
        node = next;
        path.push(node);
    }
}

fn compute_path(
    node: Name, pred: &NameMap<Name>, succ: &NameMap<Name>
) -> Vec<Name> {
    let mut result = vec![];
    result.push(node);
    compute_half_path(&mut result, node, pred);
    result.reverse();
    compute_half_path(&mut result, node, succ);
    result
}

impl Graph {
    fn new() -> Self {
        Self(NameMap::new())
    }

    fn add_directed_edge(&mut self, u: Name, v: Name) {
        self.0.entry(u).or_insert_with(NameSet::new).insert(v);
    }

    fn add_edge(&mut self, u: Name, v: Name) {
        self.add_directed_edge(u, v);
        self.add_directed_edge(v, u);
    }

    fn parse_and_add_edges(
        &mut self, table: &mut NameTable, s: &str
    ) -> Result<()> {
        let (src_str, rhs) = s.split_once(": ").ok_or("No ': ' found")?;
        let src = table.get(src_str);
        let tgts_str = rhs.split(" ");
        for tgt_str in tgts_str {
            self.add_edge(src, table.get(tgt_str));
        }
        Ok(())
    }

    fn nodes(&self) -> impl Iterator<Item = Name> + '_ {
        self.0.keys().cloned()
    }

    fn neighbors(&self, node: Name) -> Option<&NameSet> {
        self.0.get(&node)
    }

    fn dominating_set(&self, node: Name) -> NameSet {
        let mut dominating_nodes = NameSet::from([node]);
        let mut dominated_nodes = NameSet::new();
        if let Some(set) = self.neighbors(node) {
            dominated_nodes.extend(set);
        }
        let mut remaining_nodes: NameSet = self.nodes().collect();
        remaining_nodes.remove(&node);
        remaining_nodes =
            remaining_nodes.difference(&dominated_nodes).cloned().collect();
        while let Some(&node) = remaining_nodes.iter().next() {
            remaining_nodes.remove(&node);
            dominating_nodes.insert(node);
            if let Some(neighbors) = self.neighbors(node) {
                dominated_nodes.extend(neighbors);
                remaining_nodes =
                    remaining_nodes.difference(neighbors).cloned().collect();
            }
        }
        dominating_nodes
    }

    fn connected_component(&self, node: Name, residual: &EdgeSet) -> NameSet {
        let mut result = NameSet::from([node]);
        let mut queue = vec![node];
        while let Some(node) = queue.pop() {
            let Some(neighbors) = self.neighbors(node) else { continue };
            let set: NameSet =
                neighbors.difference(&result).cloned()
                .filter(|&tgt|
                    !residual.contains(&(node, tgt)) &&
                    !residual.contains(&(tgt, node))).collect();
            result.extend(&set);
            queue.extend(set);
        }
        result
    }

    fn add_neighbors(
        &self, flow: impl Fn(Name, Name) -> bool, succ: &NameMap<Name>,
        q_s: &mut Vec<Name>, pred: &mut NameMap<Name>
    ) -> AddNeighbors {
        let mut q = vec![];
        while let Some(u) = q_s.pop() {
            let Some(neighbors) = self.neighbors(u) else { continue };
            for &v in neighbors {
                if pred.contains_key(&v) || flow(u, v) {
                    continue;
                }
                pred.insert(v, u);
                if succ.contains_key(&v) {
                    return AddNeighbors::Path(compute_path(v, &pred, succ));
                }
                q.push(v);
            }
        }
        AddNeighbors::NextQueue(q)
    }

    fn bidirectional_bfs(
        &self, s: Name, t: Name, flow: &EdgeSet
    ) -> Option<Vec<Name>> {
        let mut pred = NameMap::from([(s, s)]);
        let mut q_s = vec![s];
        let mut succ = NameMap::from([(t, t)]);
        let mut q_t = vec![t];
        loop {
            if q_s.len() < q_t.len() {
                match self.add_neighbors(
                    |u, v| flow.contains(&(u, v)), &succ, &mut q_s, &mut pred
                ) {
                    AddNeighbors::Path(p) => return Some(p),
                    AddNeighbors::NextQueue(q) => {
                        if q.is_empty() {
                            return None;
                        }
                        q_s = q;
                    }
                }
            }
            else {
                match self.add_neighbors(
                    |u, v| flow.contains(&(v, u)), &pred, &mut q_t, &mut succ
                ) {
                    AddNeighbors::Path(mut p) => {
                        p.reverse();
                        return Some(p)
                    }
                    AddNeighbors::NextQueue(q) => {
                        if q.is_empty() {
                            return None;
                        }
                        q_t = q;
                    }
                }
            }
        }
    }

    fn edmonds_karp(&self, s: Name, t: Name) -> (usize, EdgeSet) {
        let mut flow = EdgeSet::new();
        let mut value = 0;
        while let Some(path) = self.bidirectional_bfs(s, t, &flow) {
            flow.extend(path.iter().cloned().zip(path.iter().skip(1).cloned()));
            value += 1;
        }
        (value, flow)
    }

    fn minimum_edge_cut(&self) -> Result<(NameSet, NameSet)> {
        match self.nodes().find_map(|node|
            Some(self.dominating_set(node)).filter(|set| set.len() >= 2)
        ) {
            None => {
                let (&node_with_min_degree, _neighbors) =
                    self.0.iter()
                        .min_by_key(|(_node, neighbors)| neighbors.len())
                        .ok_or("Empty graph")?;
                let mut other_nodes: NameSet = self.nodes().collect();
                other_nodes.remove(&node_with_min_degree);
                Ok((NameSet::from([node_with_min_degree]), other_nodes))
            }
            Some(mut set) => {
                let &v = set.iter().next().ok_or("Unexpected empty set")?;
                set.remove(&v);
                let (_value, residual) =
                    set.iter().map(|&w| self.edmonds_karp(v, w))
                    .min_by_key(|(value, _residual)| *value)
                    .ok_or("Unexpected empty set")?;
                let reachable = self.connected_component(v, &residual);
                let all_nodes: NameSet = self.nodes().collect();
                let non_reachable: NameSet =
                    all_nodes.difference(&reachable).cloned().collect();
                Ok((reachable, non_reachable))
            }
        }
    }
}


fn main() -> Result<()> {
    let mut table = NameTable::new();
    let mut graph = Graph::new();
    for line in std::io::stdin().lines() {
        graph.parse_and_add_edges(&mut table, &line?)?;
    }
    let (group1, group2) = graph.minimum_edge_cut()?;
    let result_part1 = group1.len() * group2.len();
    println!("Part 1: {result_part1}");
    Ok(())
}
