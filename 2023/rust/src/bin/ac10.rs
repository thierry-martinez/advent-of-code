fn follow_pipe(symbol: char, (dx, dy): (i64, i64)) -> Option<(i64, i64)> {
    match (symbol, dx, dy) {
        ('|', 0, _) => Some ((0, dy)),
        ('-', _, 0) => Some ((dx, 0)),
        ('F', 0, -1) => Some ((1, 0)),
        ('F', -1, 0) => Some ((0, 1)),
        ('L', 0, 1) => Some ((1, 0)),
        ('L', -1, 0) => Some ((0, -1)),
        ('J', 1, 0) => Some ((0, -1)),
        ('J', 0, 1) => Some ((-1, 0)),
        ('7', 1, 0) => Some ((0, 1)),
        ('7', 0, -1) => Some ((-1, 0)),
        _ => None,
    }
}

fn infer_pipe((dx, dy): (i64, i64), (dx2, dy2): (i64, i64)) -> char {
    match (dx, dy, dx2, dy2) {
        (0, _, 0, _) => '|',
        (_, 0, _, 0) => '-',
        (0, -1, 1, 0) |
        (-1, 0, 0, 1) => 'F',
        (0, 1, 1, 0) |
        (-1, 0, 0, -1) => 'L',
        (1, 0, 0, -1) |
        (0, 1, -1, 0) => 'J',
        (1, 0, 0, 1) |
        (0, -1, -1, 0) => '7',
        _ => panic!("infer_pipe")
    }
}

fn add_offset((x, y): (i64, i64), (dx, dy): (i64, i64)) -> (i64, i64) {
    (x + dx, y + dy)
}

fn get_point(grid: &Vec<Vec<char>>, (x, y): (i64, i64)) -> char {
    grid[y as usize][x as usize]
}

fn push_pipe(
    pipes: &mut Vec<Vec<(i64, char)>>, (x, y): (i64, i64), symbol: char
) {
    if symbol == '-' {
        return;
    }
    pipes[y as usize].push((x, symbol))
}

enum State {
    Inside(i64),
    Outside,
    Up,
    Down,
}

fn main() {
    let lines = std::io::stdin().lines();
    let grid: Vec<Vec<_>> =
        lines.map(|line| line.unwrap().chars().collect()).collect();
    let width = grid.first().unwrap().len() as i64;
    let initial_position = grid.iter().enumerate().find_map(|(y, line)|
      line.iter().enumerate().find_map(move |(x, &c)|
        (c == 'S').then_some((x as i64, y as i64))
      )
    ).unwrap();
    let mut pipes: Vec<_> = grid.iter().map(|_| Vec::new()).collect();
    let (second_position, first_offset, second_offset) =
        (-1..2).find_map(|dy|
            (-1..2).find_map(|dx| {
                let (x, y) = add_offset(initial_position, (dx, dy));
                if
                    (dx, dy) != (0, 0) && y >= 0 && y < grid.len() as i64
                    && x >= 0 && x < width
                {
                    let symbol = get_point(&grid, (x, y));
                    follow_pipe(symbol, (dx, dy)).map(|d| {
                        push_pipe(&mut pipes, (x, y), symbol);
                        ((x, y), (dx, dy), d)
                    })
                }
                else {
                    None
                }
            })
        ).unwrap();
    let mut offset = second_offset;
    let mut position = add_offset(second_position, offset);
    let mut len = 1;
    loop {
        let symbol = get_point(&grid, position);
        if symbol == 'S' {
            break;
        }
        push_pipe(&mut pipes, position, symbol);
        offset = follow_pipe(symbol, offset).unwrap();
        position = add_offset(position, offset);
        len += 1;
    }
    len += 1;
    push_pipe(&mut pipes, position, infer_pipe(offset, first_offset));
    let half_len = len / 2;
    println!("Part 1: {half_len}");
    let inside_area: i64 = pipes.into_iter().map(|mut pipes| {
        pipes.sort_by_key(|&(x, _)| x);
        let (total, _state) = pipes.iter().fold(
            (0, State::Outside),
            |(total, state), &(x, symbol)| {
                let total =
                    match state {
                        State::Inside(x0) => total + x - x0 - 1,
                        _ => total
                    };
                let state =
                    match (symbol, state) {
                        ('|', State::Inside(_)) => State::Outside,
                        ('|', State::Outside) => State::Inside(x),
                        ('L', State::Outside) => State::Down,
                        ('L', State::Inside(_)) => State::Up,
                        ('F', State::Outside) => State::Up,
                        ('F', State::Inside(_)) => State::Down,
                        ('7', State::Down) => State::Inside(x),
                        ('7', State::Up) => State::Outside,
                        ('J', State::Down) => State::Outside,
                        ('J', State::Up) => State::Inside(x),
                        _ => panic!("area"),
                    };
                (total, state)
            }
        );
        total
    }).sum();
    println!("Part 2: {inside_area}")
}
