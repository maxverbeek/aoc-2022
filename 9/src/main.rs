use std::{
    io::{stdin, BufRead},
    ops::Add,
    str::FromStr,
};

enum Direction {
    U,
    D,
    L,
    R,
}

impl FromStr for Direction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "U" => Ok(Self::U),
            "D" => Ok(Self::D),
            "L" => Ok(Self::L),
            "R" => Ok(Self::R),
            _ => Err(()),
        }
    }
}

impl Direction {
    fn diff(&self) -> Coord {
        match self {
            U => Coord { x: 0, y: 1 },
            D => Coord { x: 0, y: -1 },
            L => Coord { x: -1, y: 0 },
            R => Coord { x: 1, y: 0 },
        }
    }
}

#[derive(PartialEq, Eq)]
struct Coord {
    x: i32,
    y: i32,
}

impl Add for Coord {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Coord {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

fn main() {
    let mut head = Coord { x: 0, y: 0 };
    let mut tail = Coord { x: 0, y: 0 };

    for line in stdin().lock().lines().map(|l| l.unwrap()) {
        let mut iter = line.split(" ");
        let dirc: Direction = iter.next().unwrap().parse().unwrap();
        let count: i32 = iter.next().unwrap().parse().unwrap();
    }
}
