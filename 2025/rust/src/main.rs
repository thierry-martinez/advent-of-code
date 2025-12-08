use anyhow::anyhow;
use log;

use std::collections::{HashMap, HashSet, hash_map::Entry};
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::path::Path;
use std::str::FromStr;

fn read_lines(path: &Path) -> std::io::Result<Vec<String>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    reader.lines().collect()
}

enum Part {
    One,
    Two,
}

enum Direction {
    L,
    R,
}

impl TryFrom<char> for Direction {
    type Error = anyhow::Error;

    fn try_from(c: char) -> anyhow::Result<Self> {
        match c {
            'L' => Ok(Direction::L),
            'R' => Ok(Direction::R),
            _ => Err(anyhow!("{c} is not a valid direction")),
        }
    }
}

fn problem1(path: &Path, part: Part) -> anyhow::Result<u16> {
    let lines = read_lines(path)?;
    let mut dial: u8 = 50;
    let mut count_zero: u16 = 0;
    for line in lines {
        let mut chars = line.chars();
        let direction_char = chars.next().ok_or_else(|| anyhow!("Expected direction"))?;
        let direction: Direction = direction_char.try_into()?;
        let count_string: String = chars.collect();
        let count: u16 = count_string.parse()?;
        let icount: i16 = i16::try_from(count)?;
        let offset: i16 = match direction {
            Direction::L => -icount,
            Direction::R => icount,
        };
        let dial_count = dial as i16 + offset;
        let new_dial = dial_count.rem_euclid(100);
        let increment: u16 = match part {
            Part::One => (dial == 0).into(),
            Part::Two => (dial_count.abs() / 100) as u16 + u16::from(dial_count <= 0 && dial != 0),
        };
        count_zero = count_zero
            .checked_add(increment)
            .ok_or_else(|| anyhow!("Overflow"))?;
        log::trace!("{dial_count} {new_dial} {increment} {count_zero}");
        dial = new_dial as u8;
    }
    Ok(count_zero)
}

fn problem2(path: &Path, part: Part) -> anyhow::Result<u64> {
    let lines = read_lines(path)?;
    let line = lines.iter().next().unwrap();
    let intervals = line
        .split(',')
        .map(|interval| -> anyhow::Result<(u64, u64)> {
            let dash_index = interval.find('-').unwrap();
            let lb: u64 = interval[..dash_index].parse()?;
            let ub: u64 = interval[dash_index + 1..].parse()?;
            Ok((lb, ub))
        })
        .collect::<anyhow::Result<Vec<(u64, u64)>>>()?;
    let mut sum = 0;
    for (lb, ub) in intervals {
        for value in lb..=ub {
            let value_str = format!("{value}");
            let len = value_str.len();
            match part {
                Part::One => {
                    if len % 2 != 0 {
                        continue;
                    }
                    if value_str[..len / 2] == value_str[len / 2..] {
                        sum += value;
                    }
                }
                Part::Two => {
                    let value_bytes = value_str.as_bytes();
                    for division in 2..=len {
                        if len % division != 0 {
                            continue;
                        }
                        let mut slices = value_bytes.chunks(len / division);
                        let pattern = slices.next().unwrap();
                        if slices.all(|other_slice| pattern == other_slice) {
                            log::trace!("{value}");
                            sum += value;
                            break;
                        }
                    }
                }
            }
        }
    }
    Ok(sum)
}

fn nmax<Item>(iter: impl ExactSizeIterator<Item = Item>, n: usize) -> Vec<Item>
where
    Item: std::cmp::Ord,
{
    let mut result: Vec<Item> = Vec::new();
    let mut remaining = iter.len();
    for item in iter {
        let mut lower = n.saturating_sub(remaining);
        let mut upper = result.len();
        while lower < upper {
            let middle = lower.midpoint(upper);
            let middle_item = &result[middle];
            if middle_item < &item {
                upper = middle;
            } else {
                lower = middle + 1;
            }
        }
        if lower < n {
            result.truncate(lower);
            result.push(item);
        }
        remaining -= 1;
    }
    result
}

fn problem3(path: &Path, part: Part) -> anyhow::Result<u64> {
    let lines = read_lines(path)?;
    let n = match part {
        Part::One => 2,
        Part::Two => 12,
    };
    let joltages = lines
        .iter()
        .map(|line| -> anyhow::Result<u64> {
            let bank = line
                .chars()
                .map(|char| -> anyhow::Result<u8> { Ok(char.to_string().parse()?) })
                .collect::<anyhow::Result<Vec<u8>>>()?;
            let numbers = nmax(bank.iter().cloned(), n);
            log::trace!("{bank:?} {numbers:?}");
            Ok(numbers
                .into_iter()
                .fold(0, |accu, digit| accu * 10 + digit as u64))
        })
        .collect::<anyhow::Result<Vec<u64>>>()?;
    Ok(joltages.iter().sum())
}

fn problem4(path: &Path, part: Part) -> anyhow::Result<usize> {
    let lines = read_lines(path)?;
    let mut lines: Vec<Vec<char>> = lines
        .into_iter()
        .map(|line| line.chars().collect())
        .collect();
    let mut sum = 0;
    loop {
        let mut rolls = Vec::new();
        for (y, line) in lines.iter().enumerate() {
            for (x, char) in line.iter().enumerate() {
                if *char != '@' {
                    continue;
                }
                let mut roll_count = 0;
                for sy in y.saturating_sub(1)..=y + 1 {
                    let Some(line) = lines.get(sy) else { break };
                    for sx in x.saturating_sub(1)..=x + 1 {
                        if sy != y || sx != x {
                            let Some(&char) = line.get(sx) else { break };
                            if char == '@' {
                                roll_count += 1;
                            }
                        }
                    }
                }
                if roll_count < 4 {
                    rolls.push((x, y));
                }
            }
        }
        sum += rolls.len();
        match part {
            Part::One => break,
            Part::Two => {
                if rolls.len() == 0 {
                    break;
                }
                for (x, y) in rolls {
                    lines[y][x] = '.';
                }
            }
        }
    }
    Ok(sum)
}

fn problem5(path: &Path, part: Part) -> anyhow::Result<usize> {
    let lines = read_lines(path)?;
    let mut line_iter = lines.into_iter();
    let intervals = line_iter
        .by_ref()
        .take_while(|s| s != "")
        .map(|s| -> anyhow::Result<(usize, usize)> {
            let (low, high) = s
                .split_once('-')
                .ok_or_else(|| anyhow!("Not an interval: {s}"))?;
            Ok((low.parse()?, high.parse()?))
        })
        .collect::<anyhow::Result<Vec<(usize, usize)>>>()?;
    let result = match part {
        Part::One => {
            let ingredients: Vec<usize> = line_iter
                .map(|s| -> anyhow::Result<usize> { Ok(s.parse()?) })
                .collect::<anyhow::Result<Vec<usize>>>()?;
            ingredients
                .into_iter()
                .filter(|&ingredient| {
                    intervals
                        .iter()
                        .any(|(low, high)| *low <= ingredient && ingredient <= *high)
                })
                .count()
        }
        Part::Two => {
            let mut intervals = intervals;
            intervals.sort_by_key(|(low, _high)| *low);
            let mut count = 0;
            let mut counted_up_to = 0;
            for (low, high) in intervals {
                if counted_up_to < low {
                    count += high - low + 1;
                    counted_up_to = high;
                } else if counted_up_to < high {
                    count += high - counted_up_to;
                    counted_up_to = high;
                }
            }
            count
        }
    };
    Ok(result)
}

enum Operator {
    Add,
    Mul,
}

impl TryFrom<char> for Operator {
    type Error = anyhow::Error;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        if c == '+' {
            Ok(Operator::Add)
        } else if c == '*' {
            Ok(Operator::Mul)
        } else {
            Err(anyhow!("Unknown operator: {c}"))
        }
    }
}

fn problem6(path: &Path, part: Part) -> anyhow::Result<u64> {
    let lines = read_lines(path)?;
    let problems: Vec<(Vec<u64>, Operator)> = match part {
        Part::One => {
            let mut lines: Vec<_> = lines
                .into_iter()
                .map(|line| {
                    line.split_whitespace()
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>()
                        .into_iter()
                })
                .collect();
            lines
                .pop()
                .unwrap()
                .map(|operator| -> anyhow::Result<(Vec<u64>, Operator)> {
                    let values = lines
                        .iter_mut()
                        .map(|iter| -> anyhow::Result<u64> { Ok(iter.next().unwrap().parse()?) })
                        .collect::<anyhow::Result<Vec<u64>>>()?;
                    let operator = operator.chars().next().unwrap().try_into()?;
                    Ok((values, operator))
                })
                .collect::<anyhow::Result<Vec<(Vec<u64>, Operator)>>>()?
        }
        Part::Two => {
            let mut lines: Vec<_> = lines
                .into_iter()
                .map(|line| line.chars().collect::<Vec<char>>().into_iter())
                .collect();
            let width = lines[0].len();
            let columns: Vec<String> = (0..width)
                .map(|_| {
                    lines
                        .iter_mut()
                        .map(|iter| iter.next().unwrap())
                        .collect::<String>()
                        .trim()
                        .to_string()
                })
                .collect();
            let mut problems: Vec<Vec<String>> = vec![];
            let mut current_problem: Vec<String> = vec![];
            for column in columns.into_iter().rev() {
                if column == "" {
                    problems.push(current_problem);
                    current_problem = vec![];
                } else {
                    current_problem.push(column);
                }
            }
            problems.push(current_problem);
            problems
                .into_iter()
                .map(|mut columns| -> anyhow::Result<(Vec<u64>, Operator)> {
                    let last_column = columns.pop().unwrap();
                    let mut chars: Vec<char> = last_column.chars().collect();
                    let operator: Operator = chars.pop().unwrap().try_into()?;
                    let last_column: String = chars.iter().collect();
                    columns.push(last_column.trim().to_string());
                    let values = columns
                        .into_iter()
                        .map(|s| -> anyhow::Result<u64> { Ok(s.parse()?) })
                        .collect::<anyhow::Result<Vec<u64>>>()?;
                    Ok((values, operator))
                })
                .collect::<anyhow::Result<Vec<(Vec<u64>, Operator)>>>()?
        }
    };
    let result: u64 = problems
        .into_iter()
        .map(|(numbers, operator): (Vec<u64>, Operator)| -> u64 {
            match operator {
                Operator::Add => numbers.into_iter().sum(),
                Operator::Mul => numbers.into_iter().product(),
            }
        })
        .sum();
    Ok(result)
}

fn increment_timelines(map: &mut HashMap<usize, usize>, index: usize, count: usize) {
    match map.entry(index) {
        Entry::Occupied(mut entry) => {
            *entry.get_mut() += count;
        }
        Entry::Vacant(entry) => {
            entry.insert(count);
        }
    }
}

fn problem7(path: &Path, part: Part) -> anyhow::Result<usize> {
    let lines = read_lines(path)?;
    let mut line_iter = lines.iter();
    let entry_point = line_iter.next().unwrap().find('S').unwrap();
    let splitter_lines: Vec<HashSet<usize>> = line_iter
        .map(|line| {
            line.match_indices('^')
                .map(|m| m.0)
                .collect::<HashSet<usize>>()
        })
        .collect();
    let result = match part {
        Part::One => {
            let mut beams = HashSet::from([entry_point]);
            let mut split_count = 0;
            for splitters in splitter_lines {
                let mut new_beams = HashSet::new();
                for beam in beams {
                    if splitters.contains(&beam) {
                        new_beams.insert(beam - 1);
                        new_beams.insert(beam + 1);
                        split_count += 1;
                    } else {
                        new_beams.insert(beam);
                    }
                }
                beams = new_beams;
            }
            split_count
        }
        Part::Two => {
            let mut beams = HashMap::from([(entry_point, 1)]);
            for splitters in splitter_lines {
                let mut new_beams = HashMap::new();
                for (beam, timeline_count) in beams {
                    if splitters.contains(&beam) {
                        increment_timelines(&mut new_beams, beam - 1, timeline_count);
                        increment_timelines(&mut new_beams, beam + 1, timeline_count);
                    } else {
                        increment_timelines(&mut new_beams, beam, timeline_count);
                    }
                }
                beams = new_beams;
            }
            beams.iter().map(|(_, count)| count).sum()
        }
    };
    Ok(result)
}

struct Coordinates {
    x: u64,
    y: u64,
    z: u64,
}

impl Coordinates {
    fn distance(&self, other: &Coordinates) -> f64 {
        ((self.x as f64 - other.x as f64).powi(2)
            + (self.y as f64 - other.y as f64).powi(2)
            + (self.z as f64 - other.z as f64).powi(2))
        .sqrt()
    }
}

impl FromStr for Coordinates {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let values = s
            .split(',')
            .map(|s| -> anyhow::Result<u64> { Ok(s.parse()?) })
            .collect::<anyhow::Result<Vec<u64>>>()?;
        let &[x, y, z] = &values[..] else {
            Err(anyhow!("Wrong number of coordinates"))?
        };
        Ok(Coordinates { x, y, z })
    }
}

enum UnionFind {
    Root { size: usize },
    Link { target: usize },
}

impl UnionFind {
    fn size(&self) -> Option<usize> {
        match self {
            UnionFind::Root { size } => Some(*size),
            UnionFind::Link { .. } => None,
        }
    }
}

fn find_root(uf: &mut Vec<UnionFind>, i: usize) -> (usize, usize) {
    match uf[i] {
        UnionFind::Root { size } => (i, size),
        UnionFind::Link { target } => {
            let (r, size) = find_root(uf, target);
            uf[i] = UnionFind::Link { target: r };
            (r, size)
        }
    }
}

fn merge(uf: &mut Vec<UnionFind>, i0: usize, i1: usize) -> bool {
    let (r0, s0) = find_root(uf, i0);
    let (r1, s1) = find_root(uf, i1);
    if r0 != r1 {
        let (rmin, rmax) = if s0 < s1 { (r0, r1) } else { (r1, r0) };
        uf[rmin] = UnionFind::Link { target: rmax };
        uf[rmax] = UnionFind::Root { size: s0 + s1 };
        true
    } else {
        false
    }
}

fn problem8(path: &Path, nconnections: usize, part: Part) -> anyhow::Result<u64> {
    let lines = read_lines(path)?;
    let boxes: Vec<Coordinates> = lines
        .iter()
        .map(|line| -> anyhow::Result<Coordinates> { Ok(line.parse()?) })
        .collect::<anyhow::Result<Vec<Coordinates>>>()?;
    let mut junctions: Vec<UnionFind> = boxes.iter().map(|_| UnionFind::Root { size: 1 }).collect();
    let mut connections: Vec<((usize, usize), f64)> = boxes
        .iter()
        .enumerate()
        .flat_map(|(i0, b0): (usize, &Coordinates)| {
            boxes[0..i0]
                .iter()
                .enumerate()
                .map(move |(i1, b1)| ((i0, i1), b0.distance(b1)))
        })
        .collect();
    connections.sort_by(|(_, d0), (_, d1)| d0.partial_cmp(d1).unwrap());
    match part {
        Part::One => {
            for ((i0, i1), _distance) in &connections[0..nconnections] {
                merge(&mut junctions, *i0, *i1);
            }
            let mut sizes: Vec<usize> = junctions.iter().filter_map(|cell| cell.size()).collect();
            sizes.sort();
            Ok(sizes[sizes.len() - 3..].iter().product::<usize>() as u64)
        }
        Part::Two => {
            for ((i0, i1), _distance) in connections {
                if merge(&mut junctions, i0, i1) {
                    if junctions.iter().filter_map(|cell| cell.size()).count() == 1 {
                        return Ok(boxes[i0].x * boxes[i1].x);
                    }
                }
            }
            Err(anyhow!("Unreachable"))
        }
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let answer = problem1(Path::new("../input/1/input"), Part::One)?;
    println!("Problem 1 part 1: {answer}");
    let answer = problem1(Path::new("../input/1/input"), Part::Two)?;
    println!("Problem 1 part 2: {answer}");

    let answer = problem2(Path::new("../input/2/input"), Part::One)?;
    println!("Problem 2 part 1: {answer}");
    let answer = problem2(Path::new("../input/2/input"), Part::Two)?;
    println!("Problem 2 part 2: {answer}");

    let answer = problem3(Path::new("../input/3/input"), Part::One)?;
    println!("Problem 3 part 1: {answer}");
    let answer = problem3(Path::new("../input/3/input"), Part::Two)?;
    println!("Problem 3 part 2: {answer}");

    let answer = problem4(Path::new("../input/4/input"), Part::One)?;
    println!("Problem 4 part 1: {answer}");
    let answer = problem4(Path::new("../input/4/input"), Part::Two)?;
    println!("Problem 4 part 2: {answer}");

    let answer = problem5(Path::new("../input/5/input"), Part::One)?;
    println!("Problem 5 part 1: {answer}");
    let answer = problem5(Path::new("../input/5/input"), Part::Two)?;
    println!("Problem 5 part 2: {answer}");

    let answer = problem6(Path::new("../input/6/input"), Part::One)?;
    println!("Problem 6 part 1: {answer}");
    let answer = problem6(Path::new("../input/6/input"), Part::Two)?;
    println!("Problem 6 part 2: {answer}");

    let answer = problem7(Path::new("../input/7/input"), Part::One)?;
    println!("Problem 7 part 1: {answer}");
    let answer = problem7(Path::new("../input/7/input"), Part::Two)?;
    println!("Problem 7 part 2: {answer}");

    let answer = problem8(Path::new("../input/8/input"), 1000, Part::One)?;
    println!("Problem 8 part 1: {answer}");
    let answer = problem8(Path::new("../input/8/input"), 1000, Part::Two)?;
    println!("Problem 8 part 2: {answer}");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_problem1_part1() -> anyhow::Result<()> {
        let answer = problem1(Path::new("../input/1/example.txt"), Part::One)?;
        assert_eq!(answer, 3);
        Ok(())
    }

    #[test]
    fn test_problem1_part2() -> anyhow::Result<()> {
        let answer = problem1(Path::new("../input/1/example.txt"), Part::Two)?;
        assert_eq!(answer, 6);
        Ok(())
    }

    #[test]
    fn test_problem2() -> anyhow::Result<()> {
        let answer = problem2(Path::new("../input/2/example"), Part::One)?;
        assert_eq!(answer, 1227775554);
        let answer = problem2(Path::new("../input/2/example"), Part::Two)?;
        assert_eq!(answer, 4174379265);
        Ok(())
    }

    #[test]
    fn test_problem3() -> anyhow::Result<()> {
        env_logger::init();

        let answer = problem3(Path::new("../input/3/example"), Part::One)?;
        assert_eq!(answer, 357);

        let answer = problem3(Path::new("../input/3/example"), Part::Two)?;
        assert_eq!(answer, 3121910778619);

        Ok(())
    }

    #[test]
    fn test_problem4() -> anyhow::Result<()> {
        env_logger::init();

        let answer = problem4(Path::new("../input/4/example"), Part::One)?;
        assert_eq!(answer, 13);

        let answer = problem4(Path::new("../input/4/example"), Part::Two)?;
        assert_eq!(answer, 43);

        Ok(())
    }

    #[test]
    fn test_problem5() -> anyhow::Result<()> {
        env_logger::init();

        let answer = problem5(Path::new("../input/5/example"), Part::One)?;
        assert_eq!(answer, 3);

        let answer = problem5(Path::new("../input/5/example"), Part::Two)?;
        assert_eq!(answer, 14);

        Ok(())
    }

    #[test]
    fn test_problem6() -> anyhow::Result<()> {
        env_logger::init();

        let answer = problem6(Path::new("../input/6/example"), Part::One)?;
        assert_eq!(answer, 4277556);

        let answer = problem6(Path::new("../input/6/example"), Part::Two)?;
        assert_eq!(answer, 3263827);

        Ok(())
    }

    #[test]
    fn test_problem7() -> anyhow::Result<()> {
        env_logger::init();

        let answer = problem7(Path::new("../input/7/example"), Part::One)?;
        assert_eq!(answer, 21);

        let answer = problem7(Path::new("../input/7/example"), Part::Two)?;
        assert_eq!(answer, 40);

        Ok(())
    }

    #[test]
    fn test_problem8() -> anyhow::Result<()> {
        env_logger::init();

        let answer = problem8(Path::new("../input/8/example"), 10, Part::One)?;
        assert_eq!(answer, 40);

        let answer = problem8(Path::new("../input/8/example"), 10, Part::Two)?;
        assert_eq!(answer, 25272);

        Ok(())
    }
}
