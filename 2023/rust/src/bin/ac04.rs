fn main() {
    let lines = std::io::stdin().lines();
    let cards: Vec<_> = lines.map(
	|line| {
	    let line = line.unwrap();
            let (_, contents) = line.as_str().split_once(':').unwrap();
            let (winning_numbers_str, numbers_str) =
                contents.split_once(" | ").unwrap();
	    let winning_numbers: std::collections::HashSet<i64> =
		winning_numbers_str.split(" ")
		.filter_map(|s| s.parse().ok()).collect();
	    let winning_numbers_count =
		numbers_str.split(" ").filter_map(|s| s.parse().ok())
		.filter(|number| winning_numbers.contains(number))
		.count();
	    winning_numbers_count
	}
    ).collect();
    let result_part1: usize = cards.iter().map(
	|&count| if count == 0 { 0 } else { 1 << (count - 1) }
    ).sum();
    let mut copies = Vec::new();
    let result_part2: usize = cards.iter().map(
	|&count| {
	    let copies_count = copies.pop().unwrap_or(1);
	    let previous_copies: Vec<_> =
		(0 .. count).map(|_| copies.pop().unwrap_or(1)).collect();
	    copies.extend(previous_copies.iter().rev().map(|v| v + copies_count));
	    copies_count
	}
    ).sum();
    println!("Part 1: {result_part1}");
    println!("Part 2: {result_part2}");
}
