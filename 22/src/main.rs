use std::{
    fmt::Display,
    io::{stdin, Read},
    ops::Add,
    str::FromStr,
};

#[derive(Debug)]
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

    fn increment_cube_pos(&self, pos: &Point, dir: &Direction) -> (Point, Direction) {
        match dir {
            Direction::N => {
                if pos.y == self.tb[pos.x as usize] as isize {
                    if (0..50).contains(&pos.x) {
                        let newpos = Point {
                            x: 50,
                            y: 50 + pos.x,
                        };

                        let newdir = Direction::E;

                        return (newpos, newdir);
                    }

                    if (50..100).contains(&pos.x) {
                        let newpos = Point {
                            x: 0,
                            y: 150 + pos.x - 50,
                        };

                        let newdir = Direction::E;

                        return (newpos, newdir);
                    }

                    if (100..150).contains(&pos.x) {
                        let newpos = Point {
                            x: pos.x - 100,
                            y: 199,
                        };

                        let newdir = Direction::N;

                        return (newpos, newdir);
                    }

                    unreachable!("y: out of bounds, {}", pos.y);
                }
            }
            Direction::E => {
                if pos.x == self.rb[pos.y as usize] as isize {
                    if (0..50).contains(&pos.y) {
                        let newpos = Point {
                            x: 99,
                            y: 149 - pos.y,
                        };

                        let newdir = Direction::W;

                        return (newpos, newdir);
                    }

                    if (50..100).contains(&pos.y) {
                        let newpos = Point {
                            x: 100 + pos.y - 50,
                            y: 49,
                        };
                        let newdir = Direction::N;

                        return (newpos, newdir);
                    }

                    if (100..150).contains(&pos.y) {
                        let newpos = Point {
                            x: 149,
                            y: 149 - pos.y,
                        };

                        let newdir = Direction::W;

                        return (newpos, newdir);
                    }

                    if (150..200).contains(&pos.y) {
                        let newpos = Point {
                            x: 50 + pos.y - 150,
                            y: 149,
                        };

                        let newdir = Direction::N;

                        return (newpos, newdir);
                    }

                    unreachable!("x out of bounds: {}", pos.x);
                }
            }
            Direction::S => {
                if pos.y == self.db[pos.x as usize] as isize {
                    return match pos.x {
                        0..=49 => (
                            Point {
                                x: pos.x + 100,
                                y: 0,
                            },
                            Direction::S,
                        ),
                        50..=99 => (
                            Point {
                                x: 49,
                                y: 150 + pos.x - 50,
                            },
                            Direction::W,
                        ),
                        100..=149 => (
                            Point {
                                x: 99,
                                y: 50 + pos.x - 100,
                            },
                            Direction::W,
                        ),
                        _ => unreachable!("x out of bounds: {}", pos.x),
                    };
                }
            }
            Direction::W => {
                if pos.x == self.lb[pos.y as usize] as isize {
                    return match pos.y {
                        0..=49 => (
                            Point {
                                x: 0,
                                y: 100 + 49 - pos.y,
                            },
                            Direction::E,
                        ),
                        50..=99 => (
                            Point {
                                x: pos.y - 50,
                                y: 100,
                            },
                            Direction::S,
                        ),
                        100..=149 => (
                            Point {
                                x: 50,
                                y: 149 - pos.y,
                            },
                            Direction::E,
                        ),
                        150..=199 => (
                            Point {
                                x: 50 + pos.y - 150,
                                y: 0,
                            },
                            Direction::S,
                        ),
                        _ => unreachable!("y out of bounds: {}", pos.y),
                    };
                }
            }
        };

        let newpos = pos + &dir.as_point();

        // move 1 forward in the same direction
        return (newpos, dir.clone());
    }

    fn char_at(&self, location: &Point) -> char {
        return self.map[location.y as usize][location.x as usize];
    }

    fn surface_area(&self) -> usize {
        let mut sum = 0;

        for x in 0..self.tb.len() {
            sum += self.db[x] - self.tb[x] + 1;
        }

        sum
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Direction {
    N,
    E,
    S,
    W,
}

#[derive(Debug, Clone)]
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
            Direction::N => 3,
            Direction::E => 0,
            Direction::S => 1,
            Direction::W => 2,
        }
    }
}

#[derive(PartialEq, Debug)]
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

impl Point {
    fn scale(&self, s: isize) -> Self {
        Point {
            x: self.x * s,
            y: self.y * s,
        }
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

#[derive(Debug, Clone)]
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

fn get_border(map: &Map, edgesize: usize) -> Vec<Instruction> {
    todo!("i want to sleep instead of working on a generic edge zipping algo");
    let mut border = Vec::new();

    let mut pos = map.get_start();
    let mut dir = Direction::E;

    // from the top left coordinate, we can always go to the right first because there is always a
    // top edge
    border.push(Instruction::Walk(edgesize - 1));
    pos = &pos + &dir.as_point().scale((edgesize - 1) as isize);

    dbg!(map);

    while pos != map.get_start() {
        dbg!(&border, &pos);
        let top = map.tb[pos.x as usize] == pos.y as usize;
        let bot = map.db[pos.x as usize] == pos.y as usize;
        let left = map.lb[pos.y as usize] == pos.x as usize;
        let right = map.rb[pos.y as usize] == pos.x as usize;

        if !top && !bot && !left && !right {
            // we are in the middle of the map at an intersection. we always turn left in these
            // cases
            border.push(Instruction::Turn(TurnDirection::L));
            dir = dir.turn(TurnDirection::L);
            println!(
                "in the middle of map, {}, {}, {}, {}, {}",
                pos, top, bot, left, right
            );
        } else if (dir == Direction::N && top)
            || (dir == Direction::E && right)
            || (dir == Direction::S && bot)
            || (dir == Direction::W && left)
        {
            border.push(Instruction::Turn(TurnDirection::R));
            dir = dir.turn(TurnDirection::R);
        } else {
            // we are going straight, so walk 1 square extra
            pos = &pos + &dir.as_point();
            border.push(Instruction::Walk(1));
        }

        // continue for another edge
        pos = &pos + &dir.as_point().scale((edgesize - 1) as isize);
        border.push(Instruction::Walk(edgesize - 1));
    }

    border
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

    for i in instructions.clone() {
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

    // part 2
    let mut pos = map.get_start();
    let mut dir = Direction::E;

    for i in instructions {
        match i {
            Instruction::Walk(distance) => {
                for _ in 0..distance {
                    let (newpos, newdir) = map.increment_cube_pos(&pos, &dir);

                    if map.char_at(&newpos) == ' ' {
                        panic!(
                            "walked out of the map at {}; was at {} walking {:?}",
                            newpos, pos, dir
                        );
                    }

                    if map.char_at(&newpos) == '.' {
                        pos = newpos;
                        dir = newdir;
                    }
                }
            }
            Instruction::Turn(rotation) => dir = dir.turn(rotation),
        }
    }

    println!("final location 2: {}", pos);
    println!(
        "password 2: {}",
        (pos.y + 1) * 1000 + (pos.x + 1) * 4 + dir.as_num()
    );
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_edge() {
        println!("ok");
    }
}
