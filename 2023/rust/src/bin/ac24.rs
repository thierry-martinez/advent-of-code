use advent_of_code::{Error, Result, Coords2D, Coords3D, Matrix2D};

struct Hailstone<T> {
    p: T,
    v: T,
}

impl<T: Copy> Hailstone<T> {
    fn map<U>(&self, f: impl Fn(T) -> U) -> Hailstone<U> {
        Hailstone { p: f(self.p), v: f(self.v) }
    }
}

impl Hailstone<Coords2D<i64>> {
    fn as_f64(&self) -> Hailstone<Coords2D<f64>> {
        self.map(|c| c.map(|v| v as f64))
    }
}

impl Hailstone<Coords3D<i64>> {
    fn as_f64(&self) -> Hailstone<Coords3D<f64>> {
        self.map(|c| c.map(|v| v as f64))
    }
}

impl Hailstone<Coords2D<f64>> {
    fn intersect(&self, other: &Self) -> Option<Coords2D<f64>> {
        let d = self.v.det(other.v);
        if d == 0. {
            return None;
        }
        let t_self = (other.p.det(other.v) + other.v.det(self.p)) / d;
        let t_other = (self.p.det(self.v) + self.v.det(other.p)) / (- d);
        if t_self < 0. || t_other < 0. {
            return None;
        }
        Some (self.p + self.v * t_self)
    }
}

impl std::str::FromStr for Hailstone<Coords3D<i64>> {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let (inf_str, sup_str) = s.split_once(" @ ").ok_or("' @ ' missing")?;
        Ok(Self {
            p: Coords3D::parse(", ", inf_str)?,
            v: Coords3D::parse(", ", sup_str)?
        })
    }
}

fn part1(hailstones: &Vec<Hailstone<Coords3D<i64>>>) -> usize {
    let (min, max) = (200000000000000., 400000000000000.);
    let xy: Vec<_> = hailstones.iter().map(|s| s.map(|c| c.xy())).collect();
    xy.iter().enumerate().map(|(i, s0)|
        (&xy[i + 1 ..]).iter().filter(|s1|
            s0.as_f64().intersect(&s1.as_f64()).is_some_and(|p|
                min <= p.x && p.x <= max && min <= p.y && p.y <= max)
        ).count()
    ).sum()
}

trait TryMax {
    type Item;

    fn try_max_by(
        self,
        compare: impl FnMut(
            &Self::Item, &Self::Item
        ) -> Option<std::cmp::Ordering>
    ) -> Option<Self::Item>;

    fn try_max_by_key<B>(
        self, mut f: impl FnMut(&Self::Item) -> B
    ) -> Option<Self::Item>
    where Self: Sized, B: std::cmp::PartialOrd
    {
        self.try_max_by(|a, b| {
            let a = f(a);
            let b = f(b);
            a.partial_cmp(&b)
        })
    }
}

impl<I: Iterator> TryMax for I {
    type Item = I::Item;

    fn try_max_by(
        mut self,
        mut compare: impl FnMut(
            &Self::Item, &Self::Item
        ) -> Option<std::cmp::Ordering>
    ) -> Option<Self::Item> {
        let mut result = self.next()?;
        for item in self {
            let ord = compare(&item, &result)?;
            if ord == std::cmp::Ordering::Greater {
                result = item
            }
        }
        Some(result)
    }
}

fn gauss_jordan(matrix: &mut Matrix2D<f64>) -> Result<()> {
    let mut row_index = 0;
    for column in 0 .. matrix[0].len() {
        let (pivot_index, pivot_value) =
            (0 .. matrix.len()).map(|i| (i, matrix[i][column]))
            .try_max_by_key(|(_i, v)| v.abs()).ok_or("Unable to find a max")?;
        if pivot_value == 0. {
            continue;
        }
        let (before_row, from_row) = matrix.split_at_mut(row_index);
        let (pivot_row_slice, after_row) = from_row.split_at_mut(1);
        let pivot_row = pivot_row_slice.get_mut(0).unwrap();
        if pivot_index < row_index {
            std::mem::swap(before_row.get_mut(pivot_index).unwrap(), pivot_row)
        }
        else if pivot_index > row_index {
            std::mem::swap(
                after_row.get_mut(pivot_index - row_index - 1).unwrap(),
                pivot_row
            )
        }
        pivot_row[column] = 1.;
        for item in (&mut pivot_row[column + 1..]).iter_mut() {
            *item /= pivot_value;
        }
        for row in before_row.iter_mut().chain(after_row.iter_mut()) {
            let k = row[column];
            row[column] = 0.;
            for (cell, pivot_cell) in
                (&mut row[column + 1 ..]).iter_mut()
                .zip(&pivot_row[column + 1 ..]) {
                *cell -= k * pivot_cell;
            }
        }
        row_index += 1;
        if row_index >= matrix.len() {
            break;
        }
    }
    Ok(())
}

fn matrix_line(
    a: &Hailstone<Coords3D<f64>>, b: &Hailstone<Coords3D<f64>>
) -> Vec<f64> {
    vec![
        a.v.y - b.v.y, b.v.x - a.v.x, b.p.y - a.p.y, a.p.x - b.p.x,
        b.p.y * b.v.x - a.p.y * a.v.x + a.p.x * a.v.y - b.p.x * b.v.y
    ]
}

fn part2(hailstones: &Vec<Hailstone<Coords3D<i64>>>) -> Result<usize> {
    let hailstones: Vec<_> =
        hailstones.iter().take(5).map(Hailstone::<Coords3D<i64>>::as_f64).collect();
    let mut matrix: Vec<_> =
        (1 .. 5).map(|i| matrix_line(&hailstones[0], &hailstones[i])).collect();
    gauss_jordan(&mut matrix)?;
    let x = matrix[0][4];
    let y = matrix[1][4];
    let vx = matrix[2][4];
    let _vy = matrix[3][4];
    let a = &hailstones[0];
    let b = &hailstones[1];
    let mut matrix = vec![
      vec![vx - a.v.x, a.p.x - x, a.p.z * (vx - a.v.x) + a.v.z * (a.p.x - x)],
      vec![vx - b.v.x, b.p.x - x, b.p.z * (vx - b.v.x) + b.v.z * (b.p.x - x)]
    ];
    gauss_jordan(&mut matrix)?;
    let z = matrix[0][2];
    Ok((x + y + z) as usize)
}

fn main() -> Result<()> {
    let hailstones: Vec<Hailstone<Coords3D<i64>>> = std::io::stdin().lines()
        .map(|line| -> Result<_> { line?.parse() })
        .collect::<Result<_>>()?;
    let result_part1 = part1(&hailstones);
    println!("Part 1: {result_part1}");
    let result_part2 = part2(&hailstones)?;
    println!("Part 2: {result_part2}");
    Ok(())
}
