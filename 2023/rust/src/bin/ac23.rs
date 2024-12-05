use advent_of_code::{Result, Matrix2D, Coords2D, matrix_from_lines};

enum Neighbor {
    OneWay(Coords2D<usize>),
    BothWays(Coords2D<usize>, Coords2D<usize>),
    Intersection(usize)
}

struct Path {
    len: usize,
    end: Option<usize>,
}

type NeighborGrid = Matrix2D<Option<Neighbor>>;

fn follow_neighbor(
    neighbor_grid: &NeighborGrid, mut from: Coords2D<usize>,
    mut position: Coords2D<usize>
) -> Option<Path> {
    let mut len = 1;
    let end = Coords2D::<usize>::from(neighbor_grid)
        .checked_add_signed(Coords2D { x: -2, y: -1 })?;
    loop {
        if position == end {
            return Some(Path { len, end: None });
        }
        let next =
            match position.get(&neighbor_grid) {
                None => return None,
                Some(Neighbor::OneWay(target)) => {
                    if *target == from {
                        return None;
                    }
                    *target
                }
                Some(Neighbor::BothWays(a, b)) =>
                    if from == *a {
                        *b
                    }
                    else {
                        *a
                    },
                Some(Neighbor::Intersection(id)) =>
                    return Some(Path { len, end: Some(*id)})
            };
        from = position;
        position = next;
        len += 1;
    }
}

struct IntersectionGraph {
    initial: Path,
    paths: Vec<Vec<Path>>,
}

impl IntersectionGraph {
    fn new(grid: &Matrix2D<char>, slippy: bool) -> Result<Self> {
        let size: Coords2D<usize> = grid.into();
        let mut intersections = Vec::new();
        let neighbor_grid: NeighborGrid = grid.iter().enumerate().map(|(y, line)|
            line.iter().enumerate().map(|(x, &c)| {
                let position = Coords2D { x, y };
                match c {
                    '>' if slippy =>
                        Some(Neighbor::OneWay(
                            position + Coords2D::<usize>::RIGHT)),
                    'v' if slippy =>
                        Some(Neighbor::OneWay(
                            position + Coords2D::<usize>::DOWN)),
                    '.' | '>' | 'v' => {
                        let neighbors: Vec<_> =
                            Coords2D::<isize>::NEIGHBORS.iter()
                            .filter_map(|&direction|
                                position.advance(size, direction)
                                .filter(|position| *position.get(&grid) != '#'))
                            .collect();
                        match &neighbors[..] {
                            &[] | &[_] => None,
                            &[a, b] => Some(Neighbor::BothWays(a, b)),
                            _ => {
                                let index = intersections.len();
                                intersections.push((position, neighbors));
                                Some(Neighbor::Intersection(index))
                            }
                        }
                    }
                    _ => None,
                }
            }).collect()
        ).collect();
        let initial = follow_neighbor(
            &neighbor_grid, Coords2D { x: 1, y: 0 }, Coords2D { x: 1, y: 1 }
        ).ok_or("No initial path")?;
        let paths: Vec<Vec<_>> = intersections.iter().map(|(position, neighbors)| {
            neighbors.into_iter().filter_map(|&neighbor| {
                follow_neighbor(&neighbor_grid, *position, neighbor)
            }).collect()
        }).collect();
        Ok(Self { initial, paths })
    }

    fn search_longest_path_rec(
        &self, visited: &mut std::collections::HashSet<usize>, accu: usize, path: &Path
    ) -> Option<usize> {
        let accu = accu + path.len;
        match path.end {
            None => Some(accu),
            Some(end) =>
                if visited.contains(&end) {
                    None
                }
                else {
                    visited.insert(end);
                    let result = self.paths[end].iter().filter_map(|path|
                        self.search_longest_path_rec(visited, accu, path))
                        .max();
                    visited.remove(&end);
                    result
                }
        }
    }

    fn search_longest_path(&self) -> Result<usize> {
        Ok(self.search_longest_path_rec(
            &mut std::collections::HashSet::new(), 0, &self.initial)
            .ok_or("No path found")?)
    }
}

fn main() -> Result<()> {
    let grid = matrix_from_lines(std::io::stdin().lines())?;
    let slippy_intersection_graph = IntersectionGraph::new(&grid, true)?;
    let result_part1 = slippy_intersection_graph.search_longest_path()?;
    println!("Part 1: {result_part1}");
    let intersection_graph = IntersectionGraph::new(&grid, false)?;
    let result_part2 = intersection_graph.search_longest_path()?;
    println!("Part 2: {result_part2}");
    Ok(())
}
