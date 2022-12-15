use std::{
    collections::HashSet,
    io::{stdin, BufRead},
    ops::Range,
};

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
            (-rowrange)..(rowrange + 1)
        }
    }
}

fn main() {
    let lines = stdin().lock().lines().map(|l| l.unwrap());

    let mut range: HashSet<isize> = HashSet::new();

    let filter = vec!['x', 'y', ':', '=', ','];

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

        let distance = sensor.distance(beacon);
        let row = sensor.get_occupants_on_row(10, distance);

        range.extend(row);
    }

    println!("{:?}", range.len());
}
