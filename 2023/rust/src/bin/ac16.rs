use advent_of_code::{Result, Coords2D, Matrix2D, Zero, matrix_from_lines};

type Size = Coords2D<usize>;

type Position = Coords2D<usize>;

type Direction = Coords2D<isize>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Beam {
    position: Position,
    direction: Direction,
}

impl Beam {
    fn advance(&self, size: Size, direction: Direction) -> Option<Self> {
        let position = self.position.advance(size, direction)?;
        Some (Self { position, direction })
    }

    const INITIAL: Self = Self {
        position: Position::ZERO,
        direction: Direction::RIGHT
    };
}

struct VisitStack {
    visited: std::collections::HashSet<Beam>,
    stack: Vec<Beam>,
}

impl VisitStack {
    fn new() -> Self {
        Self {
            visited: std::collections::HashSet::new(),
            stack: Vec::new(),
        }
    }

    fn push(&mut self, beam: Beam) {
        if self.visited.contains(&beam) {
            return;
        }
        self.visited.insert(beam);
        self.stack.push(beam);
    }

    fn push_opt(&mut self, beam: Option<Beam>) {
        match beam {
            None => (),
            Some(beam) => self.push(beam),
        }
    }

    fn pop(&mut self) -> Option<Beam> {
        self.stack.pop()
    }
}

struct EnergizedGrid {
    grid: Matrix2D<bool>,
    count: usize,
}

impl EnergizedGrid {
    fn new(size: Size) -> Self {
        Self {
            grid: (0..size.y).map(|_| vec![false; size.x]).collect(),
            count: 0
        }
    }

    fn mark(&mut self, coords: Position) {
        if !coords.get(&self.grid) {
            coords.set(&mut self.grid, true);
            self.count += 1;
        }
    }
}

fn count_energized(grid: &Matrix2D<char>, initial: Beam) -> usize {
    let mut stack = VisitStack::new();
    stack.push(initial);
    let size = Size::from(grid);
    let mut energized = EnergizedGrid::new(size);
    while let Some(beam) = stack.pop() {
        energized.mark(beam.position);
        match beam.position.get(&grid) {
            '.' => stack.push_opt(beam.advance(size, beam.direction)),
            '/' =>
                stack.push_opt(
                    beam.advance(size, beam.direction.turn_anticlockwise())),
            '\\' =>
                stack.push_opt(beam.advance(size, beam.direction.turn_clockwise())),
            '-' =>
                if beam.direction.y == 0 {
                    stack.push_opt(beam.advance(size, beam.direction))
                }
                else {
                    stack.push_opt(beam.advance(size, Direction::LEFT));
                    stack.push_opt(beam.advance(size, Direction::RIGHT));
                },
            '|' =>
                if beam.direction.x == 0 {
                    stack.push_opt(beam.advance(size, beam.direction))
                }
                else {
                    stack.push_opt(beam.advance(size, Direction::UP));
                    stack.push_opt(beam.advance(size, Direction::DOWN));
                },
            _ => panic!("count_energized")
        }
    }
    energized.count
}

fn maximize_energy(grid: &Matrix2D<char>) -> usize {
    let size = Size::from(grid);
    (0..size.y).flat_map(|y|
        std::iter::once(
            Beam { position: Position { x: 0, y }, direction: Direction::RIGHT }
        ).chain(std::iter::once(
            Beam { position: Position { x: size.x - 1, y }, direction: Direction::LEFT }
        ))).chain(
    (0..size.x).flat_map(|x|
        std::iter::once(
            Beam { position: Position { x, y: 0 }, direction: Direction::DOWN }
        ).chain(std::iter::once(
            Beam { position: Position { x, y: size.y - 1 }, direction: Direction::UP }
    )))).map(|initial| count_energized(&grid, initial)).max().unwrap()
}

fn main() -> Result<()> {
    let grid: Matrix2D<char> = matrix_from_lines(std::io::stdin().lines())?;
    let result_part1 = count_energized(&grid, Beam::INITIAL);
    println!("Part 1: {result_part1}");
    let result_part2 = maximize_energy(&grid);
    println!("Part 2: {result_part2}");
    Ok(())
}
