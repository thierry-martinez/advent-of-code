use anyhow::anyhow;
use log;

use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::path::Path;

fn read_lines(path: &Path) -> std::io::Result<Vec<String>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    reader.lines().collect()
}

enum Part {
    One,
    Two
}

enum Direction {
    L,
    R
}

impl TryFrom<char> for Direction {
    type Error = anyhow::Error;

    fn try_from(c: char) -> anyhow::Result<Self> {
        match c {
            'L' => Ok(Direction::L),
            'R' => Ok(Direction::R),
            _ => Err(anyhow!("{c} is not a valid direction"))
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
        let offset: i16 =
            match direction {
                Direction::L => - icount,
                Direction::R => icount,
            };
        let dial_count = dial as i16 + offset;
        let new_dial = dial_count.rem_euclid(100);
        let increment: u16 =
            match part {
                Part::One => (dial == 0).into(),
                Part::Two =>
                (dial_count.abs() / 100) as u16 + u16::from(dial_count <= 0 && dial != 0),
            };
        count_zero = count_zero.checked_add(increment).ok_or_else(|| anyhow!("Overflow"))?;
        log::trace!("{dial_count} {new_dial} {increment} {count_zero}");
        dial = new_dial as u8;
    }
    Ok(count_zero)
}

fn problem2(path: &Path, part: Part) -> anyhow::Result<u64> {
    let lines = read_lines(path)?;
    let line = lines.iter().next().unwrap();
    let intervals = line.split(',').map(|interval| -> anyhow::Result<(u64, u64)> {
      let dash_index = interval.find('-').unwrap();
      let lb: u64 = interval[..dash_index].parse()?;
      let ub: u64 = interval[dash_index + 1..].parse()?;
      Ok((lb, ub))
    }).collect::<anyhow::Result<Vec<(u64, u64)>>>()?;
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

fn nmax<Item>(iter: impl ExactSizeIterator<Item=Item>, n: usize) -> Vec<Item>
where Item: std::cmp::Ord {
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
            }
            else {
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
    let n =
        match part {
            Part::One => 2,
            Part::Two => 12,
        };
    let joltages = lines.iter().map(|line| -> anyhow::Result<u64> {
      let bank = line.chars().map(|char| -> anyhow::Result<u8> { Ok(char.to_string().parse()?) }).collect::<anyhow::Result<Vec<u8>>>()?;
      let numbers = nmax(bank.iter().cloned(), n);
      log::trace!("{bank:?} {numbers:?}");
      Ok(numbers.into_iter().fold(0, |accu, digit| accu * 10 + digit as u64))
    }).collect::<anyhow::Result<Vec<u64>>>()?;
    Ok(joltages.iter().sum())
}

fn problem4(path: &Path, part: Part) -> anyhow::Result<usize> {
    let lines = read_lines(path)?;
    let mut lines: Vec<Vec<char>> = lines.into_iter().map(|line| line.chars().collect()).collect();
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
    let intervals = line_iter.by_ref().take_while(|s| s != "").map(|s| -> anyhow::Result<(usize, usize)> {
      let (low, high) = s.split_once('-').ok_or_else(|| anyhow!("Not an interval: {s}"))?;
      Ok((low.parse()?, high.parse()?))
    }).collect::<anyhow::Result<Vec<(usize, usize)>>>()?;
    let result =
      match part {
        Part::One => {
            let ingredients: Vec<usize> = line_iter.map(|s| -> anyhow::Result<usize> { Ok(s.parse()?) })
                .collect::<anyhow::Result<Vec<usize>>>()?;
            ingredients.into_iter().filter(|&ingredient| intervals.iter().any(|(low, high)| *low <= ingredient && ingredient <= *high)).count()
        }
        Part::Two => {
            let mut sorted_intervals: Vec<(usize, usize)> = Vec::new();
            for (low, high) in intervals {
                log::trace!("{low}-{high}");
                let mut lower = 0;
                let mut upper = sorted_intervals.len();
                while lower < upper {
                    let middle = lower.midpoint(upper);
                    let (mlow, mhigh) = sorted_intervals[middle];
                    if mhigh < low {
                        lower = middle + 1;
                    }
                    else if high < mlow {
                        upper = middle;
                    }
                    else {
                        let start =
                            if mlow <= low {
                                middle
                            }
                            else {
                                let mut start = middle;
                                while let Some(prev) = start.checked_sub(1) && sorted_intervals[prev].1 >= low {
                                    start = prev;
                                };
                                start
                            };
                        let end =
                            if high <= mhigh {
                                middle
                            }
                            else {
                                let mut end = middle;
                                while let Some((elow, _ehigh)) = sorted_intervals.get(end + 1) && *elow <= high {
                                    end += 1;
                                };
                                end
                            };
                        if start == end {
                            sorted_intervals[middle] = (std::cmp::min(low, mlow), std::cmp::max(high, mhigh));
                        }
                        else {
                            let mlow = sorted_intervals[start].0;
                            let mhigh = sorted_intervals[end].1;
                            sorted_intervals.drain(start + 1..=end);
                            sorted_intervals[start] = (std::cmp::min(low, mlow), std::cmp::max(high, mhigh));
                        }
                        break;
                    }
                }
                if lower >= upper {
                    sorted_intervals.insert(lower, (low, high));
                }
            };
            sorted_intervals.into_iter().map(|(low, high)| high - low + 1).sum()
        }
      };
    Ok(result)
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
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

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
}
