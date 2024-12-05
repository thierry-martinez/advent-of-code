use advent_of_code::{Result, Coords2D, Zero};

#[derive(Clone, Copy)]
enum Direction {
    Left,
    Right,
    Up,
    Down,
}

impl From<Direction> for Coords2D<i64> {
    fn from(direction: Direction) -> Self {
        match direction {
            Direction::Left => Coords2D::LEFT,
            Direction::Right => Coords2D::RIGHT,
            Direction::Up => Coords2D::UP,
            Direction::Down => Coords2D::DOWN,
        }
    }
}

#[derive(Clone, Copy)]
struct Line {
    direction: Direction,
    count: u64,
}

fn parse_direction_part1(c: char) -> Result<Direction> {
    match c {
        'R' => Ok(Direction::Right),
        'D' => Ok(Direction::Down),
        'L' => Ok(Direction::Left),
        'U' => Ok(Direction::Up),
        _ => Err("Invalid direction character")?,
    }
}

fn parse_direction_part2(c: char) -> Result<Direction> {
    match c {
        '0' => Ok(Direction::Right),
        '1' => Ok(Direction::Down),
        '2' => Ok(Direction::Left),
        '3' => Ok(Direction::Up),
        _ => Err("Invalid direction character")?,
    }
}

fn parse_line(line: &str) -> Result<(Line, Line)> {
    let mut parts = line.split(' ');
    let direction_part1_chars: Vec<_> =
        parts.next().ok_or("Missing direction")?.chars().collect();
    let direction_part1 =
        match &direction_part1_chars[..] {
            &[c] => parse_direction_part1(c)?,
            _ => return Err("Invalid direction symbol")?,
        };
    let count_part1 : u64 = parts.next().ok_or("Missing count")?.parse()?;
    let line_part1 = Line { direction: direction_part1, count: count_part1 };
    let color_part = parts.next().ok_or("Missing color part")?.
        strip_prefix("(#").ok_or("Missing color prefix")?;
    let color_chars_vec: Vec<_> = color_part.chars().collect();
    let mut color_chars = color_chars_vec.iter();
    let count_part2_str: String = color_chars.by_ref().take(5).collect();
    let count_part2 = u64::from_str_radix(&count_part2_str, 16)?;
    let direction_part2 : Direction =
        parse_direction_part2(*color_chars.next().ok_or("Missing direction")?)?;
    let line_part2 = Line { direction: direction_part2, count: count_part2 };
    Ok((line_part1, line_part2))
}

fn area(lines: &[Line]) -> Result<u64> {
    let mut coords: Vec<_> = lines.iter().scan(Coords2D::<i64>::ZERO,
        |pos, &line| {
            *pos = *pos + Coords2D::from(line.direction) * (line.count as i64);
            Some(*pos)
        }
    ).collect();
    coords.push(*coords.first().ok_or("Empty coords")?);
    let twice_area: i64 =
        coords.windows(2).map(|p| Coords2D::det(p[0], p[1])).sum();
    let perimeter: u64 = lines.iter().map(|line| line.count).sum();
    Ok((twice_area / 2).abs() as u64 + perimeter / 2 + 1)
}

fn main() -> Result<()> {
    let lines: Vec<_> = std::io::stdin().lines().map(|line| {
      parse_line(&line?)
    }).collect::<Result<_>>()?;
    let (lines_part1, lines_part2): (Vec<_>, Vec<_>) =
        lines.into_iter().unzip();
    let result_part1 = area(&lines_part1)?;
    println!("Part 1: {result_part1}");
    let result_part2 = area(&lines_part2)?;
    println!("Part 2: {result_part2}");
    Ok(())
}
