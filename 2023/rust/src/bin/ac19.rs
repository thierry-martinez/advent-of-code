use advent_of_code::{Error, Result, NameTable, Name};

#[derive(Clone, Copy)]
enum Category { X, M, A, S }

impl TryFrom<char> for Category {
    type Error = Error;

    fn try_from(c: char) -> Result<Self> {
        match c {
            'x' => Ok(Category::X),
            'm' => Ok(Category::M),
            'a' => Ok(Category::A),
            's' => Ok(Category::S),
            _ => Err(format!("Unknown category: {c}"))?,
        }
    }
}

#[derive(Clone, Copy)]
enum Comparison { LT, GT }

impl Comparison {
    fn compare<T: PartialOrd>(self, a: T, b: T) -> bool {
        match self {
            Comparison::LT => a < b,
            Comparison::GT => a > b,
        }
    }
}

impl TryFrom<char> for Comparison {
    type Error = Error;

    fn try_from(c: char) -> Result<Self> {
        match c {
            '<' => Ok(Comparison::LT),
            '>' => Ok(Comparison::GT),
            _ => Err(format!("Unknown comparison: {c}"))?,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Answer { Accept, Reject }

#[derive(Clone, Copy)]
enum Action {
    Send(Name),
    Answer(Answer)
}

impl Action {
    fn parse(
        table: &mut NameTable, s: &str
    ) -> Self {
        match s {
            "A" => Action::Answer(Answer::Accept),
            "R" => Action::Answer(Answer::Reject),
            _ => Action::Send(table.get(s)),
        }
    }
}

struct Rule {
    category: Category,
    comparison: Comparison,
    value: u64,
    action: Action,
}

impl Rule {
    fn parse(
        table: &mut NameTable, s: &str
    ) -> Result<Self> {
        let mut chars = s.chars();
        let category: Category =
            chars.next().ok_or("Missing category")?.try_into()?;
        let comparison: Comparison =
            chars.next().ok_or("Missing comparison")?.try_into()?;
        let (value_str, action_str) =
            chars.as_str().split_once(":").ok_or("Missing colon")?;
        let value = value_str.parse()?;
        Ok(Rule {
            category, comparison, value,
            action: Action::parse(table, action_str),
        })
    }

    fn check(&self, part: &Part<u64>) -> bool {
        self.comparison.compare(*part.get(self.category), self.value)
    }
}

struct Workflow {
    rules: Vec<Rule>,
    default: Action,
}

impl Workflow {
    fn parse(
        table: &mut NameTable, s: &str
    ) -> Result<(Name, Self)> {
        let (name_str, rule_str) = s.split_once('{').ok_or("Missing workflow")?;
        let name = table.get(name_str);
        let mut rule_chars = rule_str.chars();
        rule_chars.next_back();
        let rules_str: Vec<_> = rule_chars.as_str().split(',').collect();
        let mut rules_iter = rules_str.iter();
        let default_str = rules_iter.next_back().ok_or("Missing default")?;
        let default = Action::parse(table, default_str);
        let workflow = Workflow {
            rules: rules_iter.map(|s| Rule::parse(table, s)).
                collect::<Result<_>>()?,
            default
        };
        Ok((name, workflow))
    }

    fn get_action(&self, part: &Part<u64>) -> Action {
        match self.rules.iter().find(|rule| rule.check(part)) {
            None => self.default,
            Some(rule) => rule.action,
        }
    }
}

struct Workflows {
    map: std::collections::HashMap<Name, Workflow>,
}

impl Workflows {
    fn parse<'a>(
        table: &mut NameTable,
        it: &mut impl Iterator<Item = &'a String>,
    ) -> Result<Workflows> {
        Ok(Workflows { map:
            it.map(|line| Workflow::parse(table, &line)).collect::<Result<_>>()?
        })
    }

    fn get_answer(&self, mut name: Name, part: &Part<u64>) -> Result<Answer> {
        loop {
            let workflow = self.map.get(&name).ok_or("Unknown workflow")?;
            match workflow.get_action(&part) {
                Action::Send(target) => name = target,
                Action::Answer(answer) => return Ok(answer),
            }
        }
    }
}

#[derive(Clone)]
struct Part<T: Clone> {
    x: T,
    m: T,
    a: T,
    s: T,
}

impl TryFrom<&str> for Part<u64> {
    type Error = Error;

    fn try_from(s: &str) -> Result<Self> {
        let mut chars = s.chars();
        if chars.next() != Some('{') {
            Err("Missing {")?
        }
        if chars.next_back() != Some('}') {
            Err("Missing }")?
        }
        let components: Vec<_> = chars.as_str().split(',').collect();
        let (x, m, a, s) =
            match &components[..] {
                &[x, m, a, s] => (x, m, a, s),
                _ => Err("4 components expected")?,
            };
        Ok(Part {
            x: x.strip_prefix("x=").ok_or("x expected")?.parse()?,
            m: m.strip_prefix("m=").ok_or("m expected")?.parse()?,
            a: a.strip_prefix("a=").ok_or("a expected")?.parse()?,
            s: s.strip_prefix("s=").ok_or("s expected")?.parse()?,
        })
    }
}

impl Part<u64> {
    fn sum(&self) -> u64 {
        self.x + self.m + self.a + self.s
    }

    fn is_accepted(
        &self, table: &mut NameTable, workflows: &Workflows
    ) -> Result<bool> {
        Ok(workflows.get_answer(table.get("in"), self)? == Answer::Accept)
    }
}

impl<T: Clone> Part<T> {
    fn get(&self, c: Category) -> &T {
        match c {
            Category::X => &self.x,
            Category::M => &self.m,
            Category::A => &self.a,
            Category::S => &self.s,
        }
    }

    fn set(&mut self, c: Category, v: T) {
        match c {
            Category::X => self.x = v,
            Category::M => self.m = v,
            Category::A => self.a = v,
            Category::S => self.s = v,
        }
    }
}

#[derive(Clone, Copy)]
struct Range {
    low: u64,
    high: u64,
}

impl Range {
    fn split(
        self, comparison: Comparison, value: u64
    ) -> (Option<Range>, Option<Range>) {
        match comparison {
            Comparison::LT =>
                if self.high < value {
                    (Some(self), None)
                }
                else if self.low < value {
                    (Some(Range { low: self.low, high: value - 1 }),
                     Some(Range { low: value, high: self.high }))
                }
                else {
                    (None, Some(self))
                },
            Comparison::GT =>
                if self.low > value {
                    (Some(self), None)
                }
                else if self.high > value {
                    (Some(Range { low: value + 1, high: self.high }),
                     Some(Range { low: self.low, high: value }))
                }
                else {
                    (None, Some(self))
                }
        }
    }

    fn count(self) -> u64 {
        self.high - self.low + 1
    }
}

impl Part<Range> {
    fn count(&self) -> u64 {
        self.x.count() * self.m.count() * self.a.count() * self.s.count()
    }

    fn count_action(
        self, table: &mut NameTable, workflows: &Workflows, action: Action
    ) -> Result<u64> {
        match action {
            Action::Send(name) => self.count_accepted(table, workflows, name),
            Action::Answer(Answer::Accept) => Ok(self.count()),
            Action::Answer(Answer::Reject) => Ok(0),
        }
    }

    fn count_accepted(
        mut self, table: &mut NameTable, workflows: &Workflows, name: Name
    ) -> Result<u64> {
        let workflow = workflows.map.get(&name).ok_or("Unknown workflow")?;
        let mut sum = 0;
        for rule in &workflow.rules {
            let (yes_range, no_range) =
                self.get(rule.category).split(rule.comparison, rule.value);
            match yes_range {
                None => (),
                Some(yes_range) => {
                    let mut yes_part = self.clone();
                    yes_part.set(rule.category, yes_range);
                    sum += yes_part.count_action(table, workflows, rule.action)?;
                }
            };
            match no_range {
                None => return Ok(sum),
                Some(no_parts) => self.set(rule.category, no_parts)
            };
        }
        Ok(sum + self.count_action(table, workflows, workflow.default)?)
    }
}

fn main() -> Result<()> {
    let lines: Vec<_> = std::io::stdin().lines()
        .collect::<std::result::Result<_, _>>()?;
    let mut table = NameTable::new();
    let mut lines_iter = lines.iter();
    let mut workflow_lines =
        lines_iter.by_ref().take_while(|line| !line.is_empty());
    let workflows = Workflows::parse(&mut table, &mut workflow_lines)?;
    let parts: Vec<_> = lines_iter.map(|line| Part::try_from(line.as_str()))
        .collect::<Result<_>>()?;
    let accepted_vec: Vec<_> =
        parts.iter().map(|part|
          part.is_accepted(&mut table, &workflows).map(|b| (part, b))).
        collect::<Result<_>>()?;
    let result_part1: u64 = accepted_vec.into_iter()
        .filter_map(|(part, b)| b.then(|| part.sum())).sum();
    println!("Part 1: {result_part1}");
    let part2_range = Range { low: 1, high: 4000 };
    let initial = table.get("in");
    let result_part2: u64 =
        Part { x: part2_range, m: part2_range, a: part2_range, s: part2_range }
        .count_accepted(&mut table, &workflows, initial)?;
    println!("Part 2: {result_part2}");
    Ok(())
}
