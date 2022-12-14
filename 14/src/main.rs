use std::{
    collections::{hash_map::Entry, BTreeMap, BTreeSet, HashMap},
    error::Error,
    io::{stdin, BufRead},
    str::FromStr,
};

enum Structure {
    Sand,
    Rock,
}

struct Point {
    x: i32,
    y: i32,
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

fn main() {
    let lines = stdin().lock().lines().map(|l| l.unwrap());

    // is this a very expensive datastructure for inputs this small?
    let mut sandmap: HashMap<i32, BTreeMap<i32, Structure>> = HashMap::new();

    for line in lines {
        let mut points = line.split(" -> ").map(|s| s.parse::<Point>().unwrap());
        let mut from = points.next().expect("must have first point");

        for to in points {
            for x in from.x..=to.x {
                let rocks = (from.y..=to.y).map(|y| (y, Structure::Rock));

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

    let sand = simulate(&mut sandmap);

    println!("total sand: {}", sand); // 0 :DDD
}

fn get_sand_insert_loc(
    sandmap: &HashMap<i32, BTreeMap<i32, Structure>>,
    current: &Point,
) -> Option<Point> {
    let (&midy, _) = sandmap.get(&current.x)?.range(current.y..).next()?;

    // if we are here, the middle column is blocked at (x, midy)

    let (&lefty, _) = sandmap.get(&(current.x - 1))?.range((midy + 1)..).next()?;

    // looking down in the col to the left, the first y we encounter is not a place that occupies a
    // valid sand position
    if lefty > midy + 1 {
        return get_sand_insert_loc(
            sandmap,
            &Point {
                x: current.x - 1,
                y: lefty - 1,
            },
        );
    }

    // if we are here, the left column is also blocked
    let (&righty, _) = sandmap.get(&(current.x + 1))?.range((midy + 1)..).next()?;

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

fn simulate(sandmap: &mut HashMap<i32, BTreeMap<i32, Structure>>) -> usize {
    let sandloc = Point { x: 500, y: 0 };
    let mut numsand: usize = 0;

    loop {
        match get_sand_insert_loc(sandmap, &sandloc) {
            Some(point) => {
                let col = sandmap.get_mut(&point.x).unwrap();
                col.insert(point.y, Structure::Sand);
                numsand += 1;
            }
            None => {
                return numsand;
            }
        }
    }
}
