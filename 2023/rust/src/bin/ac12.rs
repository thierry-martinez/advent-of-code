#[derive(Clone, Copy)]
enum Symbol {
    Good,
    Bad,
    Unknown,
}

impl Symbol {
    fn from_char(c: char) -> Option<Symbol> {
        match c {
            '.' => Some(Symbol::Good),
            '#' => Some(Symbol::Bad),
            '?' => Some(Symbol::Unknown),
            _ => None,
        }
    }

    fn can_be_good(&self) -> bool {
        match self {
            Symbol::Good | Symbol::Unknown => true,
            Symbol::Bad => false,
        }
    }

    fn can_be_bad(&self) -> bool {
        match self {
            Symbol::Bad | Symbol::Unknown => true,
            Symbol::Good => false,
        }
    }
}

struct Instance {
    damaged: Vec<Symbol>,
    sequences: Vec<u64>,
}

impl Instance {
    fn parse(line: &str) -> Instance {
        let (damaged_str, sequences_str) = line.split_once(' ').unwrap();
        let damaged: Vec<_> =
            damaged_str.chars()
            .map(|c| Symbol::from_char(c).unwrap()).collect();
        let sequences: Vec<_> =
            sequences_str.split(",").map(|s| s.parse().unwrap()).collect();
        Instance { damaged, sequences }
    }

    fn align(
        &self, mut memo: &mut std::collections::HashMap<(usize, u64, usize), u64>,
        index_damaged: usize, previous_bad: u64, index_sequences: usize
    ) -> u64 {
        if let Some(&result) =
            memo.get(&(index_damaged, previous_bad, index_sequences)) {
                return result;
        }
        let result =
            match self.damaged.get(index_damaged) {
                None =>
                    match (previous_bad, &self.sequences[index_sequences..]) {
                        (0, &[]) => 1,
                        (_, &[bad]) if previous_bad == bad => 1,
                        _ => 0
                    }
                Some(symbol) => {
                    let if_good =
                        if symbol.can_be_good() {
                            if previous_bad == 0 {
                                self.align(
                                    &mut memo, index_damaged + 1, 0,
                                    index_sequences
                                )
                            }
                            else {
                                match &self.sequences[index_sequences..] {
                                    &[hd, ..] if hd == previous_bad => {
                                        self.align(
                                            &mut memo, index_damaged + 1, 0,
                                            index_sequences + 1
                                        )
                                    }
                                    _ => 0
                                }
                            }
                        }
                        else { 0 };
                    let if_bad =
                        if symbol.can_be_bad() {
                            match &self.sequences[index_sequences..] {
                                &[hd, ..] if hd > previous_bad => {
                                    self.align(
                                        &mut memo, index_damaged + 1,
                                        previous_bad + 1, index_sequences
                                    )
                                }
                                _ => 0
                            }
                        }
                        else { 0 };
                    if_good + if_bad
                }
            };
        memo.insert((index_damaged, previous_bad, index_sequences), result);
        result
    }

    fn count_alignments(&self) -> u64 {
        let mut memo = std::collections::HashMap::new();
        self.align(&mut memo, 0, 0, 0)
    }

    fn unfold(&self, times: usize) -> Self {
        Instance {
            damaged: self.damaged.iter().cloned().
                chain(std::iter::once(Symbol::Unknown)).cycle().
                take(self.damaged.len() * times + times - 1).collect(),
            sequences: self.sequences.iter().cycle().
                take(self.sequences.len() * times).cloned().collect(),
        }
    }
}

fn main() {
    let lines = std::io::stdin().lines();
    let instances: Vec<Instance> = lines.map(
        |line| {
            let line = line.unwrap();
            Instance::parse(&line)
        }
    ).collect();
    let result_part1: u64 =
        instances.iter().map(Instance::count_alignments).sum();
    println!("Part 1: {result_part1}");
    let result_part2: u64 =
        instances.iter().map(|instance| instance.unfold(5))
        .map(|instance| instance.count_alignments()).sum();
    println!("Part 2: {result_part2}");
}
