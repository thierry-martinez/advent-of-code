fn derive(numbers: &Vec<i64>) -> Vec<i64> {
    numbers.iter().zip(numbers.iter().skip(1)).map(|(a, b)| b - a).collect()
}

fn estimate(
    numbers: &Vec<i64>,
    side: &impl Fn(&Vec<i64>) -> i64,
    op: &impl Fn(i64, i64) -> i64
) -> i64 {
    if numbers.iter().all(|&n| n == 0) {
	0
    }
    else {
	let d = estimate(&derive(numbers), side, op);
	op(side(numbers), d)
    }
}

fn main() {
    let lines = std::io::stdin().lines();
    let number_lines: Vec<Vec<i64>> = lines.map(|line|
        line.unwrap().as_str().split(' ').map(|s| s.parse().unwrap()).collect()
    ).collect();
    let result_part1: i64 =
	number_lines.iter().map(
	    |numbers| estimate(numbers, &|v| *v.last().unwrap(), &|a, b| a + b)
	).sum();
    let result_part2: i64 =
	number_lines.iter().map(
	    |numbers| estimate(numbers, &|v| *v.first().unwrap(), &|a, b| a - b)
	).sum();
    println!("Part 1: {result_part1}");
    println!("Part 2: {result_part2}");
}
