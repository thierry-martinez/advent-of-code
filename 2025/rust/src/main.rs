//use anyhow::anyhow;
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
/*
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
*/
enum Part {
    One,
    Two
}
/*
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
*/

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

fn main() -> anyhow::Result<()> {
    /*
    let answer = problem1(Path::new("../input/1/input"), Part::One)?;
    println!("Problem 1 part 1: {answer}");
    let answer = problem1(Path::new("../input/1/input"), Part::Two)?;
    println!("Problem 1 part 2: {answer}");
     */
    let answer = problem2(Path::new("../input/2/input"), Part::One)?;
    println!("Problem 2 part 1: {answer}");
    let answer = problem2(Path::new("../input/2/input"), Part::Two)?;
    println!("Problem 2 part 2: {answer}");
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;
/*
    use env_logger;
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
*/
    #[test]
    fn test_problem2() -> anyhow::Result<()> {
        env_logger::init();
        let answer = problem2(Path::new("../input/2/example"), Part::One)?;
        assert_eq!(answer, 1227775554);
        let answer = problem2(Path::new("../input/2/example"), Part::Two)?;
        assert_eq!(answer, 4174379265);
        Ok(())
    }
}
