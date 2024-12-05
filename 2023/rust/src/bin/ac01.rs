fn main() {
    let lines: Vec<_> = std::io::stdin().lines().collect();
    let numbers_part1 = lines.iter().map(
	|line| {
	    let line = line.as_ref().unwrap();
	    line.chars().find(char::is_ascii_digit)
		.unwrap().to_digit(10).unwrap() * 10 +
	    line.chars().rev().find(char::is_ascii_digit)
		.unwrap().to_digit(10).unwrap()
	});
    let result_part1: u32 = numbers_part1.sum();
    let digit_numbers: Vec<_> = (0 .. 10).map(|n| (n.to_string(), n)).collect();
    let english_numbers = [
	("zero", 0),
	("one", 1),
	("two", 2),
	("three", 3),
	("four", 4),
	("five", 5),
	("six", 6),
	("seven", 7),
	("eight", 8),
	("nine", 9),
    ];
    let all_numbers: Vec<_> =
	digit_numbers.iter()
	.map(|(s, n)| (s.as_str(), *n))
	.chain(english_numbers)
	.collect();
    let numbers_part2 = lines.iter().map(
	|line| {
	    let line = line.as_ref().unwrap();
	    let left =
		all_numbers.iter().filter_map(
		    |(s, n)|
		    line.match_indices(s).next().map(|i| (i.0, n))
		).min_by_key(|(i, _n)| *i).unwrap().1;
	    let right =
		all_numbers.iter().filter_map(
		    |(s, n)| {
		    line.rmatch_indices(s).next().map(|i| (i.0, n))
		}).max_by_key(|(i, _n)| *i).unwrap().1;
	    left * 10 + right
	});
    let result_part2: u32 = numbers_part2.sum();
    println!("Part 1: {result_part1}");
    println!("Part 2: {result_part2}");
}
