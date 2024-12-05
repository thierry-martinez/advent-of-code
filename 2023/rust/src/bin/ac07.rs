fn parick_vector(s: &str) -> std::collections::HashMap<char, u64> {
    let mut result = std::collections::HashMap::new();
    s.chars().for_each(|c| {
	result.entry(c)
	    .and_modify(|e| { *e += 1 })
	    .or_insert(1);
    });
    return result
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum HandType {
    Five,
    Four,
    Full,
    Three,
    Two,
    One,
    High
}

fn hand_type_of_vec(values: &Vec<u64>) -> HandType {
    match &values[..] {
	&[5] => HandType::Five,
	&[1, 4] => HandType::Four,
	&[2, 3] => HandType::Full,
	&[1, 1, 3] => HandType::Three,
	&[1, 2, 2] => HandType::Two,
	&[1, 1, 1, 2] => HandType::One,
	&[1, 1, 1, 1, 1] => HandType::High,
	_ => panic!("impossible hand type")
    }
}

fn hand_type_part1(s: &str) -> HandType {
    let vector = parick_vector(s);
    let mut values: Vec<u64> = vector.values().cloned().collect();
    values.sort();
    hand_type_of_vec(&values)
}

fn hand_type_part2(s: &str) -> HandType {
    let mut vector = parick_vector(s);
    let joker = vector.remove(&'J').unwrap_or(0);
    let mut values: Vec<u64> = vector.values().cloned().collect();
    values.sort();
    let high_value = values.pop().unwrap_or(0) + joker;
    values.push(high_value);
    hand_type_of_vec(&values)
}

const STRENGTH_PART1: &str = "AKQJT98765432";

const STRENGTH_PART2: &str = "AKQT98765432J";

fn eval_hand_bids(
    hand_bids: &mut Vec<(String, u64)>,
    hand_type: impl Fn(&str) -> HandType,
    strength: &str
) -> u64 {
    hand_bids.sort_by_key(
	|(hand, _bid)| {
	    let values: Vec<_> =
		hand.chars().map(
		    |card|
		    strength.chars().enumerate().find(|(_, c)| *c == card)
			.unwrap().0
		).collect();
	    (hand_type(hand), values)
	}
    );
    hand_bids.iter().rev().enumerate()
	.map(|(index, (_hand, bid))| (index + 1) as u64 * bid).sum()
}

fn main() {
    let mut hand_bids: Vec<_> = std::io::stdin().lines().map(
	|line| {
	    let line = line.unwrap();
	    let hand = (&line[0..5]).to_string();
	    let bid: u64 = (&line[6..]).parse().unwrap();
	    (hand, bid)
	}
    ).collect();
    let result_part1: u64 =
	eval_hand_bids(&mut hand_bids, hand_type_part1, STRENGTH_PART1);
    let result_part2: u64 =
	eval_hand_bids(&mut hand_bids, hand_type_part2, STRENGTH_PART2);
    println!("Part 1: {result_part1}");
    println!("Part 2: {result_part2}");
}
