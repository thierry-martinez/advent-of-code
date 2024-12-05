pub struct Error(String);

impl<T: ToString> From<T> for Error {
    fn from(x: T) -> Self {
        Error(x.to_string())
    }
}

impl std::fmt::Debug for Error {
    fn fmt(
        &self, f: &mut std::fmt::Formatter<'_>
    ) -> std::result::Result<(), std::fmt::Error> {
        self.0.fmt(f)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Name(usize);

pub struct NameTable(
    std::collections::HashMap<String, Name>,
    std::collections::HashMap<Name, String>,
);

impl NameTable {
    pub fn new() -> NameTable {
        NameTable(
            std::collections::HashMap::new(),
            std::collections::HashMap::new()
        )
    }

    pub fn get(&mut self, name: &str) -> Name {
        match self.0.get(name) {
            Some(&part) => part,
            None => {
                let index = self.0.len();
                let result = Name(index);
                self.0.insert(name.to_string(), result);
                self.1.insert(result, name.to_string());
                result
            }
        }
    }

    pub fn to_str(&self, name: Name) -> Option<&str> {
        self.1.get(&name).map(|x| x.as_str())
    }
}

pub type NameSet = std::collections::HashSet<Name>;

pub type NameMap<T> = std::collections::HashMap<Name, T>;

pub fn gcd(mut a: u64, mut b: u64) -> u64 {
    while a % b != 0 {
	(a, b) = (b, a % b)
    }
    b
}

pub fn lcm(a: u64, b: u64) -> u64 {
    a * b / gcd(a, b)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Coords2D<T> {
    pub x: T,
    pub y: T,
}

pub trait Zero {
    const ZERO: Self;
}

impl Zero for usize {
    const ZERO: Self = 0;
}

impl Zero for isize {
    const ZERO: Self = 0;
}

impl Zero for i64 {
    const ZERO: Self = 0;
}

pub trait Unit {
    const UNIT: Self;
}

impl Unit for usize {
    const UNIT: Self = 1;
}

impl Unit for isize {
    const UNIT: Self = 1;
}

impl Unit for i64 {
    const UNIT: Self = 1;
}

pub trait NegUnit {
    const NEG_UNIT: Self;
}

impl NegUnit for isize {
    const NEG_UNIT: Self = -1;
}

impl NegUnit for i64 {
    const NEG_UNIT: Self = -1;
}

impl<T: Zero> Zero for Coords2D<T> {
    const ZERO: Self = Self { x: T::ZERO, y: T::ZERO };
}

impl Coords2D<usize> {
    pub fn get<'a, T>(&self, mat: &'a Matrix2D<T>) -> &'a T {
        &mat[self.y][self.x]
    }

    pub fn try_get<'a, T>(&self, mat: &'a Matrix2D<T>) -> Option<&'a T> {
        mat.get(self.y)?.get(self.x)
    }

    pub fn set<T>(&self, mat: &mut Matrix2D<T>, value: T) {
        mat[self.y][self.x] = value
    }
}

impl<T: Copy + Zero + Unit> Coords2D<T> {
    pub const RIGHT: Self = Self { x: T::UNIT, y: T::ZERO };

    pub const DOWN: Self = Self { x: T::ZERO, y: T::UNIT };
}

impl<T: Copy + std::ops::Neg<Output = T> + Zero + Unit + NegUnit> Coords2D<T> {
    pub fn turn_clockwise(&self) -> Self {
        Self { x: self.y, y: self.x }
    }

    pub fn turn_anticlockwise(&self) -> Self {
        Self { x: - self.y, y: - self.x }
    }

    pub const LEFT: Self = Self { x: T::NEG_UNIT, y: T::ZERO };

    pub const UP: Self = Self { x: T::ZERO, y: T::NEG_UNIT };

    pub const UP_LEFT: Self = Self { x: T::NEG_UNIT, y: T::NEG_UNIT };

    pub const NEIGHBORS: [Self; 4] =
        [Self::LEFT, Self::RIGHT, Self::UP, Self::DOWN];
}

impl<T> Coords2D<T> {
    pub fn det<U>(self, other: Coords2D<U>) ->
        <<T as std::ops::Mul<U>>::Output as std::ops::Sub>::Output
    where T: std::ops::Mul<U>, T::Output: std::ops::Sub {
        self.x * other.y -  self.y * other.x
    }

    pub fn map<U>(self, f: impl Fn(T) -> U) -> Coords2D<U> {
        Coords2D { x: f(self.x), y: f(self.y) }
    }
}

impl<T, U> std::ops::Add<Coords2D<U>> for Coords2D<T>
where T: std::ops::Add<U> {
    type Output = Coords2D<T::Output>;

    fn add(self, other: Coords2D<U>) -> Self::Output {
        Coords2D {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl<T, U> std::ops::Sub<Coords2D<U>> for Coords2D<T>
where T: std::ops::Sub<U> {
    type Output = Coords2D<T::Output>;

    fn sub(self, other: Coords2D<U>) -> Self::Output {
        Coords2D {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl<T, U: Copy> std::ops::Mul<U> for Coords2D<T>
where T: std::ops::Mul<U> {
    type Output = Coords2D<T::Output>;

    fn mul(self, scalar: U) -> Self::Output {
        Coords2D {
            x: self.x * scalar,
            y: self.y * scalar,
        }
    }
}

impl<T: std::cmp::PartialOrd> std::cmp::PartialOrd for Coords2D<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let x = self.x.partial_cmp(&other.x)?;
        let y = self.y.partial_cmp(&other.y)?;
        (x == y).then_some(x)
    }
}

impl Coords2D<usize> {
    pub fn checked_add_signed(&self, direction: Coords2D<isize>) -> Option<Self> {
        let x = self.x.checked_add_signed(direction.x)?;
        let y = self.y.checked_add_signed(direction.y)?;
        Some(Self { x, y })
    }

    pub fn advance(
        &self, size: Coords2D<usize>, direction: Coords2D<isize>
    ) -> Option<Self> {
        let result = self.checked_add_signed(direction)?;
        (result < size).then_some(result)
    }
}

pub type Matrix2D<T> = Vec<Vec<T>>;

impl<T> From<&Matrix2D<T>> for Coords2D<usize> {
    fn from(mat: &Matrix2D<T>) -> Self {
        Self { x: mat[0].len(), y: mat.len() }
    }
}

impl TryFrom<Coords2D<isize>> for Coords2D<usize> {
    type Error = <usize as TryFrom<isize>>::Error;

    fn try_from(c: Coords2D<isize>) -> std::result::Result<Self, Self::Error> {
        Ok(Self {x: c.x.try_into()?, y: c.y.try_into()?})
    }
}

impl TryFrom<Coords2D<usize>> for Coords2D<isize> {
    type Error = <isize as TryFrom<usize>>::Error;

    fn try_from(c: Coords2D<usize>) -> std::result::Result<Self, Self::Error> {
        Ok(Self {x: c.x.try_into()?, y: c.y.try_into()?})
    }
}

pub fn matrix_from_lines<B: std::io::BufRead>(
    lines: std::io::Lines<B>
) -> Result<Matrix2D<char>> {
    lines.map(|line| -> Result<_> { Ok(line?.chars().collect()) }).collect()
}

#[derive(Clone, Copy)]
pub struct Coords3D<T> {
    pub x: T,
    pub y: T,
    pub z: T,
}

impl<T> Coords3D<T> {
    pub fn map<U>(self, f: impl Fn(T) -> U) -> Coords3D<U> {
        Coords3D { x: f(self.x), y: f(self.y), z: f(self.z) }
    }

    pub fn try_map<U, E>(
        self,
        f: impl Fn(T) -> core::result::Result<U, E>
    ) -> core::result::Result<Coords3D<U>, E> {
        Ok(Coords3D { x: f(self.x)?, y: f(self.y)?, z: f(self.z)? })
    }

    pub fn xy(self) -> Coords2D<T> {
        Coords2D { x: self.x, y: self.y }
    }
}

impl<T: std::str::FromStr> Coords3D<T>
where T::Err: ToString {
    pub fn parse<'a>(sep: &str, s: &str) -> Result<Self> {
        let mut components = s.split(sep);
        let components_vec: Vec<_> = components.by_ref().take(3).collect();
        let coords_str =
            match &components_vec[..] {
                &[x, y, z] => Coords3D { x, y, z },
                _ => Err("Not enough components")?
            };
        if components.next().is_some() {
            Err("Too many components")?;
        }
        Ok(coords_str.try_map(str::parse)?)
    }
}
