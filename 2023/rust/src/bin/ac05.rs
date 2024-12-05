struct Line {
    dst: u64,
    src: u64,
    len: u64,
}

type Map = Vec<Line>;

struct Range {
    src: u64,
    len: u64,
}

fn main() {
    let mut lines = std::io::stdin().lines();
    let seeds: Vec<u64> =
	lines.next().unwrap().unwrap().as_str().strip_prefix("seeds: ").unwrap()
	.split(" ").map(|s| s.parse().unwrap()).collect();
    let mut lines = lines.skip(1);
    let mut maps = Vec::new();
    while let Some(_) = lines.next() {
	let mut map = Map::new();
	while let Some(line) = lines.next() {
	    let line = line.unwrap();
	    if line == "" {
		break;
	    }
	    let mut values =
                line.as_str().split(" ").map(|s| s.parse().unwrap());
	    let dst = values.next().unwrap();
	    let src = values.next().unwrap();
	    let len = values.next().unwrap();
	    map.push(Line { dst, src, len });
	}
	maps.push(map);
    }
    let mut values = seeds.clone();
    for map in &maps {
	let mut next_values = Vec::new();
	for line in map {
	    values.retain(
		|&value|
		if value >= line.src && value < line.src + line.len {
		    next_values.push(line.dst + value - line.src);
		    false
		}
		else {
		    true
		}
	    )
	}
	next_values.append(&mut values);
	values = next_values;
    }
    let result_part1 = values.iter().min().unwrap();
    let mut ranges = Vec::new();
    let mut seeds_iter = seeds.iter();
    while let Some(&src) = seeds_iter.next() {
	ranges.push(Range { src, len: *seeds_iter.next().unwrap() });
    }
    for map in maps.iter() {
	let mut next_ranges = Vec::new();
	for line in map {
	    let mut remaining_ranges = Vec::new();
	    ranges.retain(
		|range|
		if
		    range.src + range.len < line.src ||
		    line.src + line.len < range.src
		{
		    true
		}
		else {
		    let range_end = range.src + range.len;
		    let src = std::cmp::max(range.src, line.src);
		    let end = std::cmp::min(range_end, line.src + line.len);
		    next_ranges.push(
			Range { src: src + line.dst - line.src, len: end - src });
		    if src > range.src {
			remaining_ranges.push(
			    Range { src: range.src, len: src - range.src });
		    }
		    if end < range_end {
			remaining_ranges.push(
			    Range { src: end, len: range_end - end });
		    }
		    false
		}
	    );
	    ranges.append(&mut remaining_ranges);
	}
	next_ranges.append(&mut ranges);
	ranges = next_ranges;
    }
    let result_part2 = ranges.iter().map(|range| range.src).min().unwrap();
    println!("Part 1: {result_part1}");
    println!("Part 2: {result_part2}");
}
