use std::{
    collections::HashMap,
    io::{stdin, BufRead},
    ops::{Add, Range, Sub},
    vec,
};

#[derive(Clone, Copy, Debug, PartialEq)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn distance(&self, other: Self) -> isize {
        return (self.x - other.x).abs() + (self.y - other.y).abs();
    }

    fn get_occupants_on_row(&self, row: isize, radius: isize) -> Range<isize> {
        let distance = (self.y - row).abs();
        let rowrange = radius - distance;

        if rowrange < 0 {
            0..0
        } else {
            (self.x - rowrange)..(self.x + rowrange + 1)
        }
    }

    fn scale(&self, factor: isize) -> Self {
        Point {
            x: self.x * factor,
            y: self.y * factor,
        }
    }

    fn unit(&self) -> Self {
        Point {
            x: self.x.clamp(-1, 1),
            y: self.y.clamp(-1, 1),
        }
    }
}

impl Add for Point {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Sub for Point {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Point {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

#[derive(PartialEq, Debug)]
enum PointType {
    Blank,
    Beacon,
}

#[derive(Debug)]
struct Sensor {
    x: isize,
    y: isize,
    radius: isize,
}

impl Sensor {
    fn from_point(point: &Point, radius: isize) -> Self {
        Sensor {
            x: point.x,
            y: point.y,
            radius,
        }
    }

    fn distance(&self, other: &Self) -> isize {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

impl Sub for &Sensor {
    type Output = Point;

    fn sub(self, rhs: Self) -> Self::Output {
        Point {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

fn main() {
    let lines = stdin().lock().lines().map(|l| l.unwrap());

    let mut range: HashMap<isize, PointType> = HashMap::new();

    let target = 2000000;
    // let target = 10;

    let filter = vec!['x', 'y', ':', '=', ','];

    let mut sensors: Vec<Sensor> = vec![];

    for line in lines {
        let mut nums = line
            .split_whitespace()
            .map(|word| word.trim_matches(filter.as_slice()))
            .flat_map(|w| w.parse::<isize>().ok());

        let sx = nums.next().unwrap();
        let sy = nums.next().unwrap();
        let bx = nums.next().unwrap();
        let by = nums.next().unwrap();

        let sensor = Point { x: sx, y: sy };
        let beacon = Point { x: bx, y: by };

        if beacon.y == target {
            range.insert(beacon.x, PointType::Beacon);
        }

        let distance = sensor.distance(beacon);

        sensors.push(Sensor::from_point(&sensor, distance));

        for idx in sensor.get_occupants_on_row(target, distance) {
            range.entry(idx).or_insert(PointType::Blank);
        }
    }

    println!(
        "{:?}",
        range
            .iter()
            .filter(|(_idx, e)| **e == PointType::Blank)
            .count()
    );

    // for part 2, derive 2 equations of the lines that describe the gaps between two diamonds
    // (radiusses of sensors) and solve them.
    let mut equations: Vec<Equation> = vec![];

    for i in 0..sensors.len() {
        let si = &sensors[i];

        for j in (i + 1)..sensors.len() {
            let sj = &sensors[j];

            if si.distance(sj) == 2 + si.radius + sj.radius {
                let dir = (sj - si).unit();

                let sx = si.x + dir.x * (si.radius + 1);
                let sy = si.y;

                let line_origin = Point { x: sx, y: sy };
                let line_direction = Point {
                    x: -dir.x,
                    y: dir.y,
                };

                equations.push((line_origin, line_direction));
            }
        }
    }

    for i in 0..equations.len() {
        for j in (i + 1)..equations.len() {
            if let Some(Point { x, y }) = intersect(equations[i], equations[j]) {
                println!("intersect x, y: {}, {} -> {} ", x, y, x * 4000000 + y);
            }
        }
    }
}

type Equation = (Point, Point);

fn intersect(eq1: Equation, eq2: Equation) -> Option<Point> {
    // check orthogonaltiy of directions -> must be orthogonal for intersction
    if eq1.1.x * eq2.1.x + eq1.1.y * eq2.1.y != 0 {
        return None;
    }

    // i rewrite this shit to y = ax + b because im dumb
    let b = {
        let t = eq1.0.x / eq1.1.x;
        let eq = eq1.0 - eq1.1.scale(t);
        eq.y
    };

    let a = eq1.1.x * eq1.1.y;

    // do the same thing for eq2: y = cx + d
    let d = {
        let t = eq2.0.x / eq2.1.x;
        let eq = eq2.0 - eq2.1.scale(t);
        eq.y
    };

    let c = eq2.1.x * eq2.1.y;

    if a - c == 0 {
        // no intersection, or lines are vertical, except we dont have vertical lines
        return None;
    }

    let x = (d - b) / (a - c);

    Some(Point { x, y: a * x + b })
}

#[cfg(test)]
mod tests {
    use super::*;

    // these testcases reflect my willpower to solve this analytically and show my lack of math
    // skill while trying to equate two lines

    #[test]
    fn test_intersect() {
        let p1 = Point { x: 0, y: 0 }; // intersect
        let p2 = Point { x: 1, y: 1 }; // direction

        let p3 = Point { x: 0, y: 4 }; // intersect
        let p4 = Point { x: 1, y: -1 }; // direction

        // should intersect at (2, 2)
        assert_eq!(intersect((p1, p2), (p3, p4)), Some(Point { x: 2, y: 2 }));
        assert_eq!(intersect((p3, p4), (p1, p2)), Some(Point { x: 2, y: 2 }));
    }

    #[test]
    fn test_intersect_upwards() {
        let p1 = Point { x: 0, y: 0 }; // intersect
        let p2 = Point { x: 1, y: 1 }; // direction

        let p3 = Point { x: 4, y: 0 }; // intersect
        let p4 = Point { x: -1, y: 1 }; // direction

        // should intersect at (2, 2)
        assert_eq!(intersect((p1, p2), (p3, p4)), Some(Point { x: 2, y: 2 }));
        assert_eq!(intersect((p3, p4), (p1, p2)), Some(Point { x: 2, y: 2 }));
    }
}
