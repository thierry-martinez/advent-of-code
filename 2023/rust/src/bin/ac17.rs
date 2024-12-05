use advent_of_code::{
    Error, Result, Matrix2D, Coords2D, Zero, matrix_from_lines
};

#[derive(Clone, Copy,PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Direction {
    Horizontal,
    Vertical,
}

impl Direction {
    fn turn(self) -> Self {
        match self {
            Direction::Horizontal => Direction::Vertical,
            Direction::Vertical => Direction::Horizontal,
        }
    }
}

#[derive(Clone, PartialEq, Eq)] 
struct State {
    heat_loss: u64,
    pos: Coords2D<usize>,
    dir: Direction,
}

impl State {
    fn as_tuple(&self) -> (std::cmp::Reverse<u64>, usize, usize, Direction) {
        (std::cmp::Reverse(self.heat_loss), self.pos.x, self.pos.y, self.dir)
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_tuple().cmp(&other.as_tuple())
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn find_path(
    grid: &Matrix2D<u32>, min_turn: u64, max_forward: u64,
    from: Coords2D<usize>, to: Coords2D<usize>
) -> Result<u64> {
    let mut heap = std::collections::BinaryHeap::new();
    let mut heat_losses = std::collections::HashMap::new();
    heap.push(State { heat_loss: 0, pos: from, dir: Direction::Horizontal });
    heat_losses.insert((from, Direction::Horizontal), 0);
    heap.push(State { heat_loss: 0, pos: from, dir: Direction::Vertical });
    heat_losses.insert((from, Direction::Vertical), 0);
    loop {
        let state = heap.pop().ok_or("Empty queue")?;
        if state.pos == to {
            return Ok(state.heat_loss);
        }
        let heat_loss =
            heat_losses.get(&(state.pos, state.dir))
            .ok_or("Position not found")?;
        if *heat_loss != state.heat_loss {
            continue;
        }
        let dir = state.dir.turn();
        let offsets: [Coords2D<isize>; 2] =
            match dir {
                Direction::Vertical => [Coords2D::UP, Coords2D::DOWN],
                Direction::Horizontal => [Coords2D::LEFT, Coords2D::RIGHT],
            };
        for offset in offsets {
            let mut pos = state.pos;
            let mut heat_loss = state.heat_loss;
            for step in 1..=max_forward {
                let Some(next_pos) = pos.advance(grid.into(), offset)
                else { break };
                pos = next_pos;
                heat_loss += *pos.get(grid) as u64;
                if step >= min_turn {
                    match heat_losses.entry((pos, dir)) {
                        std::collections::hash_map::Entry::Vacant(vacant) => {
                            vacant.insert(heat_loss);
                        }
                        std::collections::hash_map::Entry::Occupied(mut occupied) => {
                            let cell = occupied.get_mut();
                            if *cell <= heat_loss {
                                continue;
                            }
                            *cell = heat_loss;
                        }
                    }
                    heap.push(State { heat_loss, pos, dir });
                }
            }
        }
    }
}

fn main() -> Result<()> {
    let char_grid = matrix_from_lines(std::io::stdin().lines())?;
    let grid: Matrix2D<u32> = char_grid.iter().map(|line|
      line.iter().map(|c| c.to_digit(10)
          .ok_or(Error::from(format!("Invalid digit {c}"))))
      .collect::<Result<_>>()
    ).collect::<Result<_>>()?;
    let from: Coords2D<usize> = Coords2D::ZERO;
    let to: Coords2D<usize> =
        Coords2D::from(&grid).checked_add_signed(Coords2D::UP_LEFT)
        .ok_or("Empty grid")?;
    let result_part1 = find_path(&grid, 0, 3, from, to)?;
    println!("Part 1: {result_part1}");
    let result_part2 = find_path(&grid, 4, 10, from, to)?;
    println!("Part 2: {result_part2}");
    Ok(())
}
