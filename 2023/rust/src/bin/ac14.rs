type Grid = Vec<Vec<char>>;

fn drop_rounded_rocks(
    grid: &mut Grid, column: usize, row: usize, count: usize,
    transform: &impl Fn(usize, usize) -> (usize, usize)
) {
    for i in row .. row + count {
        let (x, y) = transform(column, i);
        grid[y][x] = 'O';
    }
}

fn tilt_column(
    grid: &mut Grid, height: usize, column: usize,
    transform: &impl Fn(usize, usize) -> (usize, usize)
) {
    let mut rounded_rock_count = 0;
    for row in (0 .. height).rev() {
        let (x, y) = transform(column, row);
        match grid[y][x] {
            'O' => {
                rounded_rock_count += 1;
                grid[y][x] = '.'
            }
            '#' => {
                drop_rounded_rocks(
                    grid, column, row + 1, rounded_rock_count, transform
                );
                rounded_rock_count = 0
            }
            '.' => (),
            _ => panic!("tilt_column")
        }
    }
    drop_rounded_rocks(grid, column, 0, rounded_rock_count, transform);
}

fn tilt(
    grid: &mut Grid, height: usize, width: usize,
    transform: &impl Fn(usize, usize) -> (usize, usize)
) {
    for column in 0 .. width {
        tilt_column(grid, height, column, transform)
    }
}

fn tilt_north(grid: &mut Grid) {
    let height = grid.len();
    let width = grid[0].len();
    tilt(grid, height, width, &|i, j| (i, j));
}

fn tilt_cycle(grid: &mut Grid) {
    let height = grid.len();
    let width = grid[0].len();
    tilt(grid, height, width, &|i, j| (i, j));
    tilt(grid, width, height, &|i, j| (j, i));
    tilt(grid, height, width, &|i, j| (i, height - j - 1));
    tilt(grid, width, height, &|i, j| (width - j - 1, i));
}

fn amount_of_load(grid: &Grid) -> usize {
    grid.iter().enumerate().map(|(i, line)|
      line.iter().filter(|&&c| c == 'O').count() * (grid.len() - i)
    ).sum()
}

fn measure_cycle<'a>(
    arena: &'a typed_arena::Arena<Grid>,
    grid: Grid,
    map: &mut std::collections::HashMap<&'a Grid, usize>,
    seq: &mut Vec<&'a Grid>,
) -> (usize, usize) {
    let mut grid_ref = arena.alloc(grid);
    loop {
        match map.entry(grid_ref) {
            std::collections::hash_map::Entry::Occupied(occupied) => {
                return (*occupied.get(), seq.len());
            }
            std::collections::hash_map::Entry::Vacant(vacant) => {
                vacant.insert(seq.len());
            }
        }
        seq.push(grid_ref);
        let mut new_grid = grid_ref.clone();
        tilt_cycle(&mut new_grid);
        grid_ref = arena.alloc(new_grid);
    }
}

fn iterate_tilt(grid: Grid, count: usize) -> usize {
    let arena = typed_arena::Arena::new();
    let mut map = std::collections::HashMap::new();
    let mut seq = Vec::new();
    let (initial, cycle) = measure_cycle(&arena, grid, &mut map, &mut seq);
    let grid =
        if count < cycle {
            seq[count]
        }
        else {
            let left = (count - cycle) % (cycle - initial);
            seq[initial + left]
        };
    amount_of_load(grid)
}

fn main() {
    let lines = std::io::stdin().lines();
    let grid: Grid =
        lines.map(|line| line.unwrap().chars().collect()).collect();
    let mut grid_part1 = grid.clone();
    tilt_north(&mut grid_part1);
    let result_part1 = amount_of_load(&grid_part1);
    println!("Part 1: {result_part1}");
    let result_part2 = iterate_tilt(grid, 1000000000);
    println!("Part 2: {result_part2}");
}
