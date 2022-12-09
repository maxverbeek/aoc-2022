use std::{
    collections::HashSet,
    io::{stdin, BufRead},
    iter,
    ops::{Add, Sub},
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
            Self::U => Coord { x: 0, y: 1 },
            Self::D => Coord { x: 0, y: -1 },
            Self::L => Coord { x: -1, y: 0 },
            Self::R => Coord { x: 1, y: 0 },
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
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

impl Sub for Coord {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Coord {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl Coord {
    fn magnitude(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }

    fn chebyshev(&self, other: &Self) -> i32 {
        (self.x - other.x).abs().max((self.y - other.y).abs())
    }
}

fn calctailmove(head: &Coord, tail: &Coord) -> Coord {
    if head.chebyshev(tail) > 1 {
        return Coord {
            x: (head.x - tail.x).clamp(-1, 1),
            y: (head.y - tail.y).clamp(-1, 1),
        };
    }

    return Coord { x: 0, y: 0 };
}

fn parse_moves(input: &str) -> impl Iterator<Item = Coord> {
    let mut iter = input.split(" ");
    let dirc: Direction = iter.next().unwrap().parse().unwrap();
    let count: i32 = iter.next().unwrap().parse().unwrap();

    (0..count).map(move |_| dirc.diff())
}

fn main() {
    let mut knots = [Coord { x: 0, y: 0 }; 10];

    let instructions = stdin().lock().lines().map(|l| l.unwrap());
    let mut moves: Vec<Coord> = instructions.flat_map(|l| parse_moves(&l)).collect();
    let mut knotpositions: Vec<HashSet<Coord>> = iter::repeat(HashSet::new()).take(10).collect();

    for m in moves.iter() {
        for i in 0..9 {
            let head = knots[i];
            let mut tail = knots[i + 1];

            let head = head + *m;
            let tailmove = calctailmove(&head, &tail);

            if tailmove.magnitude() > 0 {
                tail = tail + tailmove;
                knotpositions[i + 1].insert(tail);
            }
        }
    }
    for i in 0..9 {
        let mut head = knots[i];
        let mut tail = knots[i + 1];

        let mut newmoves: Vec<Coord> = Vec::new();
        let mut set: HashSet<Coord> = HashSet::new();

        for m in moves.iter() {
            head = head + *m;
            let tailmove = calctailmove(&head, &tail);

            if tailmove.magnitude() > 0 {
                newmoves.push(tailmove);
                tail = tail + tailmove;
            }
            set.insert(tail);
        }

        moves = newmoves;

        println!("knot {} visited {} spaces", i, set.len());
    }
}
