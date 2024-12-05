fn partial_sums(it: impl Iterator<Item = u64>) -> impl Iterator<Item = u64> {
    it.scan(0, |state, x| {
	*state += x;
	Some(*state)
    })
}

fn distances(
    galaxies: &Vec<(usize, usize)>,
    empty_rows: &Vec<u64>,
    empty_columns: &Vec<u64>,
    factor: u64
) -> u64 {
    galaxies.iter().enumerate().map(|(i, &(x1, y1))|
        (&galaxies[i+1..]).iter().map(|&(x2, y2)|
            (x1 as i64 - x2 as i64).abs() as u64 +
	    (y1 as i64 - y2 as i64).abs() as u64 + 
            (
		(empty_rows[y1] as i64 - empty_rows[y2] as i64).abs() +
		(empty_columns[x1] as i64 - empty_columns[x2] as i64).abs()
	    ) as u64 * factor
	  ).sum::<u64>()
    ).sum()
}

fn main() {
    let lines = std::io::stdin().lines();
    let grid: Vec<Vec<_>> =
	lines.map(|line| line.unwrap().chars().map(|c| c == '#').
        collect()).collect();
    let width = grid.first().unwrap().len();
    let empty_rows: Vec<_> =
	partial_sums(
	    grid.iter().map(|line| line.iter().all(std::ops::Not::not) as u64)
	).collect();
    let empty_columns: Vec<_> =
	partial_sums(
	    (0..width).map(|index|
                grid.iter().all(|line| !line[index]) as u64
	    )
	).collect();
    let galaxies: Vec<_> =
	grid.iter().enumerate().flat_map(|(y, line)|
          line.iter().enumerate().filter_map(move |(x, &galaxy)|
            galaxy.then_some((x, y))
	  )).collect();
    let result_part1 = distances(&galaxies, &empty_rows, &empty_columns, 1);
    let result_part2 =
	distances(&galaxies, &empty_rows, &empty_columns, 1000000 - 1);
    println!("Part 1: {result_part1}");
    println!("Part 2: {result_part2}");
}
