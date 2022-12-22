use std::{
    fmt::Display,
    io::{stdin, Read},
    ops::Add,
    str::FromStr,
};

struct Map {
    map: Vec<Vec<char>>,

    // {top, down, left, right} bounds
    tb: Vec<usize>,
    db: Vec<usize>,
    lb: Vec<usize>,
    rb: Vec<usize>,
}

fn parse_input(maze: &str) -> Map {
    let width = maze.split("\n").map(|l| l.len()).max().unwrap();
    let height = maze.split("\n").count();

    println!("w: {}, h: {}", width, height);

    let mut map = Map {
        map: vec![vec![' '; width]; height],
        tb: vec![height; width],
        db: vec![0; width],
        lb: vec![width; height],
        rb: vec![0; height],
    };

    for (y, xs) in maze.split("\n").enumerate() {
        for (x, c) in xs.chars().enumerate() {
            map.map[y][x] = c;

            if c != ' ' {
                if y < map.tb[x] {
                    map.tb[x] = y;
                }

                if y > map.db[x] {
                    map.db[x] = y;
                }

                if x < map.lb[y] {
                    map.lb[y] = x;
                }

                if x > map.rb[y] {
                    map.rb[y] = x;
                }
            }
        }
    }

    map
}

impl Map {
    fn get_start(&self) -> Point {
        Point {
            y: 0isize,
            x: self.lb[0] as isize,
        }
    }

    fn increment_pos(&self, pos: &Point, dir: &Direction) -> Point {
        let mut newpos = pos + &dir.as_point();

        match dir {
            Direction::N => {
                if newpos.y == self.tb[newpos.x as usize] as isize - 1 {
                    newpos.y = self.db[newpos.x as usize] as isize;
                }
            }
            Direction::E => {
                if newpos.x == self.rb[newpos.y as usize] as isize + 1 {
                    newpos.x = self.lb[newpos.y as usize] as isize;
                }
            }

            Direction::S => {
                if newpos.y == self.db[newpos.x as usize] as isize + 1 {
                    newpos.y = self.tb[newpos.x as usize] as isize;
                }
            }

            Direction::W => {
                if newpos.x == self.lb[newpos.y as usize] as isize - 1 {
                    newpos.x = self.rb[newpos.y as usize] as isize;
                }
            }
        }

        newpos
    }

    fn char_at(&self, location: &Point) -> char {
        return self.map[location.y as usize][location.x as usize];
    }
}

#[derive(Debug)]
enum Direction {
    N,
    E,
    S,
    W,
}

#[derive(Debug)]
enum TurnDirection {
    L,
    R,
}

impl Direction {
    fn turn(&self, rot: TurnDirection) -> Self {
        match (rot, &self) {
            (TurnDirection::L, Self::N) => Self::W,
            (TurnDirection::L, Self::E) => Self::N,
            (TurnDirection::L, Self::S) => Self::E,
            (TurnDirection::L, Self::W) => Self::S,
            (TurnDirection::R, Self::N) => Self::E,
            (TurnDirection::R, Self::E) => Self::S,
            (TurnDirection::R, Self::S) => Self::W,
            (TurnDirection::R, Self::W) => Self::N,
        }
    }

    fn as_point(&self) -> Point {
        match &self {
            Direction::N => Point { y: -1, x: 0 },
            Direction::E => Point { x: 1, y: 0 },
            Direction::S => Point { y: 1, x: 0 },
            Direction::W => Point { x: -1, y: 0 },
        }
    }

    fn as_num(&self) -> isize {
        match &self {
            Direction::N => 4,
            Direction::E => 0,
            Direction::S => 1,
            Direction::W => 2,
        }
    }
}

struct Point {
    x: isize,
    y: isize,
}

impl Add for &Point {
    type Output = Point;

    fn add(self, rhs: &Point) -> Self::Output {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

#[derive(Debug)]
enum Instruction {
    Walk(usize),
    Turn(TurnDirection),
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(num) = s.parse::<usize>() {
            return Ok(Self::Walk(num));
        }

        if s == "L" {
            return Ok(Self::Turn(TurnDirection::L));
        }

        if s == "R" {
            return Ok(Self::Turn(TurnDirection::R));
        }

        Err(format!("bad instruction string {}", s))
    }
}

fn parse_instructions(input: &str) -> Vec<Instruction> {
    let inputinstr = input
        .match_indices(&['L', 'R'][..])
        .collect::<Vec<(usize, &str)>>();

    let mut instructions = Vec::new();

    let mut prev = 0;

    for (pos, _) in inputinstr {
        if pos == prev {
            continue;
        }

        instructions.push(input[prev..pos].parse::<Instruction>().unwrap());
        instructions.push(input[pos..=pos].parse::<Instruction>().unwrap());
        prev = pos + 1;
    }

    instructions.push(input[prev..input.len() - 1].parse::<Instruction>().unwrap());

    instructions
}

fn main() {
    let mut buf = String::new();
    stdin().lock().read_to_string(&mut buf).unwrap();

    let mut bufiter = buf.split("\n\n");

    let maze = bufiter.next().unwrap();
    let directions = bufiter.next().unwrap();
    let instructions = parse_instructions(directions);

    let map = parse_input(maze);

    let mut pos = map.get_start();
    let mut dir = Direction::E;

    println!(
        "map bounds\n{:?}\n{:?}\n{:?}\n{:?}",
        map.tb, map.rb, map.db, map.lb
    );

    for i in instructions {
        match i {
            Instruction::Walk(distance) => {
                for _ in 0..distance {
                    let newpos = map.increment_pos(&pos, &dir);

                    if map.char_at(&newpos) == ' ' {
                        panic!(
                            "walked out of the map at {}; was at {} walking {:?}",
                            newpos, pos, dir
                        );
                    }

                    if map.char_at(&newpos) == '.' {
                        pos = newpos;
                    }
                }
            }
            Instruction::Turn(rotation) => dir = dir.turn(rotation),
        }
    }

    println!("final location: {}", pos);
    println!(
        "password 1: {}",
        (pos.y + 1) * 1000 + (pos.x + 1) * 4 + dir.as_num()
    );
}
