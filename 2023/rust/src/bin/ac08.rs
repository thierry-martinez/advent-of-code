use advent_of_code::lcm;

fn main() {
    let mut lines = std::io::stdin().lines();
    let directions = lines.next().unwrap().unwrap();
    let map: std::collections::HashMap<String, (String, String)> = lines.skip(1).map(
	|line| {
	    let line = line.unwrap();
	    let (src, sides) = line.as_str().split_once(" = (").unwrap();
	    let (left, end) = sides.split_once(", ").unwrap();
	    let mut chars = end.chars();
	    chars.next_back();
	    let right = chars.as_str();
	    (src.to_string(), (left.to_string(), right.to_string()))
	}
    ).collect();
    let result_part1: u64 =
	path_length(&map, &mut directions.chars().cycle(), "AAA", |place| place == "ZZZ");
    let ghosts = map.keys().filter(|place| place.chars().last().unwrap() == 'A');
    // Not a general solution, but crafted for the kind of inputs AoC give...
    let result_part2: u64 =
	ghosts.map(
	    |place| path_length(
		&map, &mut directions.chars().cycle(), place,
		|place| place.chars().last().unwrap() == 'Z'
	    )
	).fold(1, lcm);
    println!("Part 1: {result_part1}");
    println!("Part 2: {result_part2}");
}

fn path_length(
    map: &std::collections::HashMap<String, (String, String)>,
    directions: &mut impl Iterator<Item = char>,
    start: &str,
    is_end: impl Fn(&str) -> bool
) -> u64 {
    let mut place = start;
    let mut count = 0;
    while !is_end(place) {
	let (left, right) = map.get(place).unwrap();
	place =
	    match directions.next().unwrap() {
		'L' => &left,
		'R' => &right,
		_ => panic!("unknown direction"),
	    };
	count += 1;
    }
    count
}
