fn range_has_symbol(line: &Vec<char>, start: usize, end: usize) -> bool {
    return
	(&line[std::cmp::max(start, 1) - 1..std::cmp::min(end + 1, line.len())])
	.iter().any(|c| *c != '.');
}

fn extend_number_left(line: &Vec<char>, mut x: usize) -> usize {
    while x > 0 && line[x - 1].is_digit(10) {
	x -= 1;
    }
    return x;
}

fn add_part_number_left(
    part_numbers: &mut Vec<u64>, line: &Vec<char>, x: usize
) {
    if x <= 0 || !line[x - 1].is_digit(10) {
	return;
    }
    let start = extend_number_left(line, x - 1);
    let number_str: String = (&line[start .. x]).iter().collect();
    part_numbers.push(number_str.parse().unwrap())
}

fn extend_number_right(line: &Vec<char>, mut x: usize) -> usize {
    while x < line.len() - 1 && line[x + 1].is_digit(10) {
	x += 1;
    }
    return x;
}

fn add_part_number_right(
    part_numbers: &mut Vec<u64>, line: &Vec<char>, x: usize
) {
    if x >= line.len() - 1 || !line[x + 1].is_digit(10) {
	return;
    }
    let end = extend_number_right(line, x + 1);
    let number_str: String = (&line[x + 1 .. end + 1]).iter().collect();
    part_numbers.push(number_str.parse().unwrap())
}

fn add_part_numbers_line(
    part_numbers: &mut Vec<u64>, line: &Vec<char>, x: usize
) {
    if line[x].is_digit(10) {
	let start = extend_number_left(line, x);
	let end = extend_number_right(line, x);
	let number_str: String = (&line[start .. end + 1]).iter().collect();
	part_numbers.push(number_str.parse().unwrap())
    }
    else {
	add_part_number_left(part_numbers, line, x);
	add_part_number_right(part_numbers, line, x);
    }
}

fn main() {
    let grid: Vec<Vec<char>> = std::io::stdin().lines().map(|line| line.unwrap().chars().collect()).collect();
    let mut result_part1: u64 = 0;
    grid.iter().enumerate().for_each(
	|(y, line)| {
	    let mut iter = line.iter().enumerate();
	    while let Some((x, c)) = iter.find(|(_x, c)| c.is_digit(10)) {
		let number_str: String = std::iter::once(c).chain(
		    iter.by_ref().take_while(|(_x, c)| c.is_digit(10))
			.map(|(_x, c)| c)).collect();
		let len = number_str.len();
		let marked =
		    x > 0 && line[x - 1] != '.' ||
		    x + len < line.len() && line[x + len] != '.' ||
		    y > 0 && range_has_symbol(&grid[y - 1], x, x + len) ||
		    y < grid.len() - 1 && range_has_symbol(&grid[y + 1], x, x + len);
		if marked {
		    result_part1 += number_str.parse::<u64>().unwrap();
		}
	    }
	}
    );
    println!("Part 1: {result_part1}");
    let mut result_part2 = 0;
    grid.iter().enumerate().for_each(
	|(y, line)| {
	    let mut iter = line.iter().enumerate();
	    while let Some((x, _c)) = iter.find(|(_x, c)| **c == '*') {
		let mut part_numbers = Vec::new();
		add_part_number_left(&mut part_numbers, &grid[y], x);
		add_part_number_right(&mut part_numbers, &grid[y], x);
		if y > 0 {
		    add_part_numbers_line(&mut part_numbers, &grid[y - 1], x);
		}
		if y < grid.len() - 1 {
		    add_part_numbers_line(&mut part_numbers, &grid[y + 1], x);
		}
		match &part_numbers[..] {
		    &[a, b] => result_part2 += a * b,
		    _ => (),
		}
	    }
	}
    );
    println!("Part 2: {result_part2}");
}
