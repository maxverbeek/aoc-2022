use std::{
    collections::{hash_map::Entry, BinaryHeap, HashMap, HashSet, VecDeque},
    fmt::Display,
    io::{stdin, BufRead},
};

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct Point {
    x: isize,
    y: isize,
}

impl From<(isize, isize)> for Point {
    fn from(p: (isize, isize)) -> Self {
        Self { x: p.0, y: p.1 }
    }
}

impl Point {
    fn add_dir_wrapping(&self, dir: &Direction, width: isize, height: isize) -> Self {
        match dir {
            Direction::E => ((self.x + 1).rem_euclid(width), self.y).into(),
            Direction::W => ((self.x - 1).rem_euclid(width), self.y).into(),
            Direction::N => (self.x, (self.y - 1).rem_euclid(height)).into(),
            Direction::S => (self.x, (self.y + 1).rem_euclid(height)).into(),
        }
    }

    fn in_bounds(&self, width: isize, height: isize) -> bool {
        (0..width).contains(&self.x) && (0..height).contains(&self.y)
    }

    fn add_dir(&self, dir: &Direction) -> Self {
        match dir {
            Direction::E => ((self.x + 1), self.y).into(),
            Direction::W => ((self.x - 1), self.y).into(),
            Direction::N => (self.x, (self.y - 1)).into(),
            Direction::S => (self.x, (self.y + 1)).into(),
        }
    }

    fn chebyshev(&self, other: &Point) -> isize {
        (self.x - other.x).abs().max((self.y - other.y).abs())
    }

    fn to_prio_entry(&self, end: &Point, time: usize) -> PrioEntry {
        PrioEntry {
            dist_to_end: self.chebyshev(end) as usize,
            time,
            location: *self,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum Direction {
    N,
    E,
    S,
    W,
}

impl Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Direction::N => write!(f, "^"),
            Direction::E => write!(f, ">"),
            Direction::S => write!(f, "v"),
            Direction::W => write!(f, "<"),
        }
    }
}

impl TryFrom<char> for Direction {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '>' => Ok(Self::E),
            '<' => Ok(Self::W),
            '^' => Ok(Self::N),
            'v' => Ok(Self::S),
            _ => Err(()),
        }
    }
}

fn read_input() -> (HashMap<Point, Vec<Direction>>, Point, Point, isize, isize) {
    let mut lines = stdin().lock().lines().map(|l| l.unwrap()).peekable();

    let mut initial = HashMap::new();

    let first = lines.next().unwrap();
    let start = first.find('.').unwrap() - 1;

    let mut y = 0;

    while let Some(line) = lines.peek() {
        if line.starts_with("##") {
            break;
        }

        let line = lines.next().unwrap();
        let len = line.len();

        let mut x = 0;

        for c in line[1..(len - 1)].chars() {
            if let Ok(dir) = Direction::try_from(c) {
                initial.insert((x, y).into(), vec![dir]);
            }

            x += 1;
        }

        y += 1;
    }

    let last = lines.next().unwrap();
    let stop = last.find('.').unwrap() - 1;

    return (
        initial,
        (start as isize, -1).into(),
        (stop as isize, y).into(),
        first.len() as isize - 2,
        y,
    );
}

fn draw_map(map: &HashMap<Point, Vec<Direction>>, width: isize, height: isize) {
    for y in 0..height {
        for x in 0..width {
            if let Some(bliz) = map.get(&(x, y).into()) {
                if bliz.len() == 1 {
                    print!("{}", bliz[0]);
                } else {
                    print!("{}", bliz.len());
                }
            } else {
                print!(".");
            }
        }

        print!("\n");
    }
}

fn next_blizzard(
    map: &HashMap<Point, Vec<Direction>>,
    width: isize,
    height: isize,
) -> HashMap<Point, Vec<Direction>> {
    let mut res: HashMap<Point, Vec<Direction>> = HashMap::new();

    for (point, dirs) in map {
        for dir in dirs {
            let np = point.add_dir_wrapping(dir, width, height);

            match res.entry(np) {
                Entry::Occupied(mut p) => {
                    p.get_mut().push(*dir);
                }
                Entry::Vacant(e) => {
                    e.insert(vec![*dir]);
                }
            }
        }
    }

    res
}

fn get_blizzards(
    t: usize,
    blizzards: &mut Vec<HashMap<Point, Vec<Direction>>>,
    width: isize,
    height: isize,
) -> &HashMap<Point, Vec<Direction>> {
    while t >= blizzards.len() {
        blizzards.push(next_blizzard(
            &blizzards[blizzards.len() - 1],
            width,
            height,
        ));
    }

    &blizzards[t]
}

#[derive(PartialEq, Eq)]
struct PrioEntry {
    dist_to_end: usize,
    time: usize,
    location: Point,
}

impl Ord for PrioEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let mycost = self.dist_to_end + self.time;
        let othercost = other.dist_to_end + other.time;

        othercost
            .cmp(&mycost)
            .then_with(|| other.time.cmp(&self.time))
    }
}

impl PartialOrd for PrioEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let mycost = self.dist_to_end + self.time;
        let othercost = other.dist_to_end + other.time;

        Some(
            othercost
                .cmp(&mycost)
                .then_with(|| other.time.cmp(&self.time)),
        )
    }
}

fn bfs(
    blizzards: &mut Vec<HashMap<Point, Vec<Direction>>>,
    width: isize,
    height: isize,
    start: Point,
    end: Point,
    start_time: usize,
) -> usize {
    // rather than using a queue, i use two hashsets that i alternate like a double-buffer type
    // thing. a queue didn't work for me because it was too hard keeping track of already visited
    // states, and the duplicate states racked up too much time.
    let mut visits: HashSet<Point> = HashSet::new();
    let mut nextvisits: HashSet<Point> = HashSet::new();

    nextvisits.insert(start);
    let mut time = start_time;

    while !nextvisits.contains(&end) {
        let blizz = get_blizzards(time + 1, blizzards, width, height);

        (visits, nextvisits) = (nextvisits, visits);
        nextvisits.clear();

        for pos in &visits {
            // try going in all directions
            for dir in &[Direction::N, Direction::E, Direction::S, Direction::W] {
                let nextpos = pos.add_dir(dir);

                if (nextpos.in_bounds(width, height) && blizz.get(&nextpos) == None)
                    || nextpos == end
                {
                    nextvisits.insert(nextpos);
                }
            }

            // try waiting as well
            if blizz.get(&pos) == None {
                nextvisits.insert(*pos);
            }
        }

        time += 1;
    }

    time
}

fn main() {
    let mut blizzards: Vec<HashMap<Point, Vec<Direction>>> = vec![];

    let (initial, start, end, width, height) = read_input();
    blizzards.push(initial);

    let time = bfs(&mut blizzards, width, height, start, end, 0);
    let backtime = bfs(&mut blizzards, width, height, end, start, time);
    let finishtime = bfs(&mut blizzards, width, height, start, end, backtime);

    println!("min time: {}", time);
    println!("going many times: {}", finishtime);
}
