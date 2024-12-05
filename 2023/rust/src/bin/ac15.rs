fn hash(s: &str) -> usize {
    s.chars().fold(0, |v, c| (v + c as usize) * 17 % 256)
}

enum Command {
    Remove,
    SetFocus(usize)
}

struct Instruction<'a, C> {
    label: &'a str,
    command: C
}

impl<'a> Instruction<'a, Command> {
    fn parse(s: &'a str) -> Self {
        match s.strip_suffix('-') {
            Some(label) => Self { label, command: Command::Remove },
            None => {
                let (label, focus) = s.split_once('=').unwrap();
                let command = Command::SetFocus(focus.parse().unwrap());
                Self { label, command }
            }   
        }
    }

    fn strip_remove(
        &self, removed: &mut std::collections::HashSet<&'a str>
    ) -> Option<Instruction<'a, usize>> {
        match self.command {
            Command::Remove => {
                removed.insert(self.label);
                None
            }
            Command::SetFocus(focus) => {
                if removed.contains(self.label) {
                    None
                }
                else {
                    Some(Instruction { label: self.label, command: focus })
                }
            }
        }
    }
}

impl<'a> Instruction<'a, usize> {
    fn execute(&self, state: &mut State<'a>) {
        match state.focuses.insert(self.label, self.command) {
            Some(_) => (),
            None => state.boxes[hash(self.label)].push(self.label)
        }
    }
}

struct State<'a> {
    boxes: [Vec<&'a str>; 256],
    focuses: std::collections::HashMap<&'a str, usize>,
}

impl<'a> State<'a> {
    fn new() -> Self {
        Self {
            boxes: std::array::from_fn(|_| Vec::new()),
            focuses: std::collections::HashMap::new(),
        }
    }

    fn evaluate(&self) -> usize {
        self.boxes.iter().enumerate().map(|(i, boxes)| -> usize {
          (i + 1) * boxes.iter().enumerate().map(|(j, label)|
            (j + 1) * self.focuses.get(label).unwrap()
          ).sum::<usize>()
        }).sum()
    }
}

fn main() {
    let mut input: String = String::new();
    for line in std::io::stdin().lines() {
        input.push_str(&line.unwrap())
    }
    let instructions_str: Vec<_> = input.as_str().split(',').collect();
    let result_part1: usize = instructions_str.iter().cloned().map(hash).sum();
    println!("Part 1: {result_part1}");
    let instructions = instructions_str.iter().cloned().map(Instruction::parse);
    let mut removed = std::collections::HashSet::new();
    let set_instructions: Vec<_> =
        instructions.rev().filter_map(
            |instruction| instruction.strip_remove(&mut removed)
        ).collect();
    let mut state = State::new();
    for set_instruction in set_instructions.iter().rev() {
        set_instruction.execute(&mut state);
    }
    let result_part2: usize = state.evaluate();
    println!("Part 2: {result_part2}");
}
