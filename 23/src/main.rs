use std::{
    collections::{
        hash_map::Entry::{Occupied, Vacant},
        HashMap, VecDeque,
    },
    io::{stdin, Read},
};

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }

    fn checkdirections(&self, dir: &Direction) -> [Self; 3] {
        match dir {
            Direction::N | Direction::S => {
                let y = if dir == &Direction::N {
                    self.y - 1
                } else {
                    self.y + 1
                };
                [
                    Self { x: self.x - 1, y },
                    Self { x: self.x, y },
                    Self { x: self.x + 1, y },
                ]
            }
            Direction::E | Direction::W => {
                let x = if dir == &Direction::E {
                    self.x + 1
                } else {
                    self.x - 1
                };

                [
                    Self { x, y: self.y - 1 },
                    Self { x, y: self.y },
                    Self { x, y: self.y + 1 },
                ]
            }
        }
    }

    fn move_dir(&self, dir: &Direction) -> Self {
        match dir {
            Direction::N => Self::new(self.x, self.y - 1),
            Direction::E => Self::new(self.x + 1, self.y),
            Direction::S => Self::new(self.x, self.y + 1),
            Direction::W => Self::new(self.x - 1, self.y),
        }
    }

    fn surrounding(&self) -> impl Iterator<Item = Self> + '_ {
        (-1..=1)
            .flat_map(move |y| (-1..=1).map(move |x| Point { x, y }))
            .filter(|p| *p != Point { x: 0, y: 0 })
            .map(|offset| Self {
                x: self.x + offset.x,
                y: self.y + offset.y,
            })
    }
}

#[derive(Debug, PartialEq)]
enum Direction {
    N,
    E,
    S,
    W,
}

fn propose_position(
    elves: &HashMap<Point, bool>,
    directions: &[Direction],
) -> HashMap<Point, Vec<Point>> {
    let mut res: HashMap<Point, Vec<Point>> = HashMap::new();

    for (loc, _) in elves.iter() {
        // check if any elves on surrounding squares. if none are there, do not move
        if loc.surrounding().flat_map(|p| elves.get(&p)).count() == 0 {
            continue;
        }

        // if there are any on surrounding squares, attempt to move in a proposed direction
        // whichever is available first
        for dir in directions {
            if loc
                .checkdirections(dir)
                .iter()
                .flat_map(|p| elves.get(&p))
                .count()
                == 0
            {
                // no elves are in this direction, propose to move in this direction
                let proposal = loc.move_dir(dir);
                match res.entry(proposal) {
                    Occupied(mut v) => {
                        v.get_mut().push(*loc);
                    }
                    Vacant(e) => {
                        e.insert(vec![*loc]);
                    }
                }

                // only insert 1 proposal
                break;
            }
        }
    }

    res
}

fn getbounds(elves: &HashMap<Point, bool>) -> Option<(isize, isize, isize, isize)> {
    if elves.len() == 0 {
        return None;
    }

    let mut iter = elves.iter();
    let (first, _) = iter.next().unwrap();

    let mut top = first.y;
    let mut bot = first.y;
    let mut left = first.x;
    let mut right = first.x;

    for (el, _) in iter {
        top = top.min(el.y);
        bot = bot.max(el.y);
        left = left.min(el.x);
        right = right.max(el.x);
    }

    Some((top, bot, left, right))
}

fn calcsurface(elves: &HashMap<Point, bool>) -> usize {
    if let Some((top, bot, left, right)) = getbounds(elves) {
        let width = (right - left) as usize + 1;
        let height = (bot - top) as usize + 1;
        let surface = width * height - elves.len();

        surface
    } else {
        0
    }
}

fn printmap(elves: &HashMap<Point, bool>) {
    if let Some((top, bot, left, right)) = getbounds(elves) {
        for y in top..=bot {
            for x in left..=right {
                if elves.contains_key(&Point::new(x, y)) {
                    print!("#");
                } else {
                    print!(".");
                }
            }
            print!("\n");
        }
    }
}

fn main() {
    let mut buf = String::new();
    stdin().lock().read_to_string(&mut buf).unwrap();

    let mut map = HashMap::new();

    for (y, xs) in buf.split("\n").enumerate() {
        for (x, c) in xs.chars().enumerate() {
            if c == '#' {
                let point = Point::new(x as isize, y as isize);
                map.insert(point, true);
            }
        }
    }

    let mut dirproposal =
        Into::<VecDeque<Direction>>::into([Direction::N, Direction::S, Direction::W, Direction::E]);

    // println!("initial");
    // printmap(&map);

    let mut round = 1;

    for _ in 0.. {
        let mut someonemoved = false;

        let newpositions = propose_position(&map, dirproposal.make_contiguous());

        // iterate over all elements without conflicts
        for (&dest, src) in newpositions.iter().filter(|(_, v)| v.len() == 1) {
            // move src to dest
            map.remove(&src[0]);
            map.insert(dest, true);

            someonemoved = true;
        }

        // change order of the elements
        dirproposal.rotate_left(1);

        // println!("after round {}", round);
        // printmap(&map);

        if !someonemoved {
            break;
        } else {
            round += 1;
        }
    }

    println!("surface total: {}", calcsurface(&map));
    println!("total roudns: {}", round);
}
