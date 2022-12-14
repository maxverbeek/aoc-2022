use std::{
    collections::{hash_map::Entry, BTreeMap, HashMap},
    error::Error,
    fmt::Display,
    io::{stdin, BufRead},
    ops::RangeInclusive,
    str::FromStr,
};

enum Structure {
    Sand,
    Rock,
}

#[derive(PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn x_range(&self, other: &Point) -> RangeInclusive<i32> {
        self.x.min(other.x)..=(self.x.max(other.x))
    }

    fn y_range(&self, other: &Point) -> RangeInclusive<i32> {
        self.y.min(other.y)..=(self.y.max(other.y))
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.x, self.y)?;
        Ok(())
    }
}

impl FromStr for Point {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(",");

        Ok(Point {
            x: iter.next().ok_or("no x")?.parse()?,
            y: iter.next().ok_or("no y")?.parse()?,
        })
    }
}

fn draw_map(sandmap: &HashMap<i32, BTreeMap<i32, Structure>>, minx: i32, maxx: i32, maxy: i32) {
    for y in 0..=maxy {
        for x in minx..=maxx {
            match sandmap.get(&x).map(|c| c.get(&y)).flatten() {
                None => print!("."),
                Some(Structure::Rock) => print!("#"),
                Some(Structure::Sand) => print!("o"),
            }
        }

        print!("\n");
    }
}

fn main() {
    let lines = stdin().lock().lines().map(|l| l.unwrap());
    let mut minx: i32 = 500;
    let mut maxx: i32 = 500;
    let mut maxy: i32 = 0;

    // is this a very expensive datastructure for inputs this small?
    let mut sandmap: HashMap<i32, BTreeMap<i32, Structure>> = HashMap::new();

    for line in lines {
        let mut points = line.split(" -> ").map(|s| s.parse::<Point>().unwrap());
        let mut from = points.next().expect("must have first point");

        for to in points {
            minx = minx.min(from.x).min(to.x);
            maxx = maxx.max(from.x).max(to.x);
            maxy = maxy.max(from.y).max(to.y);

            for x in from.x_range(&to) {
                let rocks = from.y_range(&to).map(|y| (y, Structure::Rock));

                match sandmap.entry(x) {
                    // If something already exists on this x, add additional rocks to it
                    Entry::Occupied(mut m) => {
                        m.get_mut().extend(rocks);
                    }

                    // if there is nothing at this x-coord yet, create a new BTreeMap filled with
                    // rocks
                    Entry::Vacant(v) => {
                        v.insert(rocks.collect());
                    }
                };
            }

            from = to;
        }
    }

    let sand = simulate(&mut sandmap, minx, maxx, maxy);

    println!("total sand with void: {}", sand);

    // add a floor
    for x in (minx - maxy - 1)..=(maxx + maxy + 1) {
        match sandmap.entry(x) {
            Entry::Occupied(mut m) => {
                m.get_mut().insert(maxy + 2, Structure::Rock);
            }
            Entry::Vacant(m) => {
                m.insert([(maxy + 2, Structure::Rock)].into());
            }
        }
    }

    // Continue the simulation with a capped simulate function
    let moresand = simulate(&mut sandmap, minx, maxx, maxy);

    println!("total sand with floor: {}", sand + moresand);
}

fn get_sand_insert_loc(
    sandmap: &HashMap<i32, BTreeMap<i32, Structure>>,
    current: &Point,
) -> Option<Point> {
    let (&midy, _) = sandmap.get(&current.x)?.range(current.y..).next()?;

    // println!("midy: {}", midy);

    // if we are here, the middle column is blocked at (x, midy)

    let (&lefty, _) = sandmap.get(&(current.x - 1))?.range(midy..).next()?;

    // println!("lefty: {}", lefty);

    // looking down in the col to the left, the first y we encounter is not a place that occupies a
    // valid sand position
    if lefty > midy {
        return get_sand_insert_loc(
            sandmap,
            &Point {
                x: current.x - 1,
                y: lefty - 1,
            },
        );
    }

    // if we are here, the left column is also blocked
    let (&righty, _) = sandmap.get(&(current.x + 1))?.range(midy..).next()?;

    // println!("righty: {}", lefty);

    if righty > midy {
        return get_sand_insert_loc(
            sandmap,
            &Point {
                x: current.x + 1,
                y: righty - 1,
            },
        );
    }

    // otherwise if the right col also does not have space, we dump it on top of the middle col
    Some(Point {
        x: current.x,
        y: midy - 1,
    })
}

fn simulate(
    sandmap: &mut HashMap<i32, BTreeMap<i32, Structure>>,
    minx: i32,
    maxx: i32,
    maxy: i32,
) -> usize {
    let sandloc = Point { x: 500, y: 0 };
    let mut numsand: usize = 0;

    loop {
        match get_sand_insert_loc(sandmap, &sandloc) {
            Some(point) => {
                // println!("inserting sand on {}", point);
                let col = sandmap.get_mut(&point.x).unwrap();
                col.insert(point.y, Structure::Sand);
                numsand += 1;

                if point == (Point { x: 500, y: 0 }) {
                    draw_map(sandmap, minx - maxy - 2, maxx + maxy + 2, maxy + 3);
                    return numsand;
                }
            }
            None => {
                return numsand;
            }
        }
    }
}
