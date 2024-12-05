use advent_of_code::{
    Result, Matrix2D, Coords2D, matrix_from_lines
};

fn next_positions(
    grid: &impl Fn(Coords2D<isize>) -> bool, p: Coords2D<isize>
) -> impl Iterator<Item = Coords2D<isize>> + '_ {
    [Coords2D::<isize>::UP, Coords2D::RIGHT, Coords2D::DOWN, Coords2D::LEFT]
        .into_iter().map(move |off| p + off).filter(|&p| grid(p))
}

fn next_position_set(
    grid: &impl Fn(Coords2D<isize>) -> bool,
    set: &std::collections::HashSet<Coords2D<isize>>
) -> std::collections::HashSet<Coords2D<isize>> {
    set.iter().flat_map(|&p| next_positions(&grid, p)).collect()
}

fn part1(grid: &Matrix2D<bool>, gardener: Coords2D<usize>) -> Result<usize> {
    let mut set = std::collections::HashSet::from([
        Coords2D::<isize>::try_from(gardener)?
    ]);
    for _ in 0 .. 64 {
        set = next_position_set(&|p|
            Coords2D::<usize>::try_from(p).is_ok_and(|p|
              p.try_get(grid).is_some_and(|&b| b)
            ), &set
        );
    }
    Ok(set.len())
}

fn modulus(a: isize, b: usize) -> usize {
    if a < 0 {
        (b as isize + (a + 1) % b as isize - 1) as usize
    }
    else {
        a as usize % b
    }
}

fn part2(grid: &Matrix2D<bool>, gardener: Coords2D<usize>) -> Result<usize> {
    let mut set = std::collections::HashSet::from([
        Coords2D::<isize>::try_from(gardener)?
    ]);
    let mut accu = Vec::new();
    let mut count = 0;
    let n = 26501365 / grid.len();
    loop {
        if count % grid.len() == grid.len() / 2 {
            accu.push(set.len());
            if let &[a, b, c] = &accu[..] {
                return Ok(
                    (n * n * (a + c - 2 * b) + n * (4 * b - 3 * a - c) + 2 * a)
                        / 2
                )
            }
        }
        count += 1;
        set = next_position_set(&|p|
            *Coords2D {
                x: modulus(p.x, grid[0].len()), y: modulus(p.y, grid.len())
            }.get(grid),
            &set
        );
    }
}

fn main() -> Result<()> {
    let char_grid = matrix_from_lines(std::io::stdin().lines())?;
    let gardener = char_grid.iter().enumerate().find_map(|(y, line)|
      line.iter().enumerate().find_map(|(x, &c)|
        (c == 'S').then_some(Coords2D { x, y })
      )
    ).ok_or("No gardener found")?;
    let grid =
        char_grid.into_iter().map(|line|
            line.into_iter().map(|c| c == '.' || c == 'S').collect()
        ).collect();
    let result_part1 = part1(&grid, gardener)?;
    println!("Part 1: {result_part1}");
    let result_part2 = part2(&grid, gardener)?;
    println!("Part 2: {result_part2}");
    Ok(())
}
