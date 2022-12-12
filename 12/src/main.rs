use std::{
    collections::VecDeque,
    io::{stdin, BufRead},
    ops::Index,
};

#[derive(Debug, PartialEq, Eq, Clone)]
struct Coord {
    y: isize,
    x: isize,
}

struct Grid {
    lenx: usize,
    leny: usize,
    map: Vec<Vec<char>>,
    visited: Vec<Vec<bool>>,
}

impl From<Vec<Vec<char>>> for Grid {
    fn from(map: Vec<Vec<char>>) -> Self {
        let lenx = map[0].len();
        let leny = map.len();

        Self {
            lenx,
            leny,
            map,
            visited: vec![vec![false; lenx]; leny],
        }
    }
}

impl Grid {
    fn find_start(&self) -> Coord {
        for (y, ys) in self.map.iter().enumerate() {
            for (x, c) in ys.iter().enumerate() {
                if *c == 'S' {
                    return Coord {
                        x: x as isize,
                        y: y as isize,
                    };
                }
            }
        }

        unreachable!();
    }

    fn find_end(&self) -> Coord {
        for (y, ys) in self.map.iter().enumerate() {
            for (x, c) in ys.iter().enumerate() {
                if *c == 'E' {
                    return Coord {
                        x: x as isize,
                        y: y as isize,
                    };
                }
            }
        }

        unreachable!();
    }

    fn in_bounds(&self, coord: &Coord) -> bool {
        coord.x >= 0
            && coord.x < (self.lenx as isize)
            && coord.y >= 0
            && coord.y < (self.leny as isize)
    }

    fn visit(&mut self, coord: &Coord) {
        self.visited[coord.y as usize][coord.x as usize] = true;
    }

    fn new_neighbours(&mut self, me: &Coord) -> Vec<Coord> {
        let current = self[me];

        let coords: Vec<Coord> = [(-1, 0), (1, 0), (0, -1), (0, 1)]
            .into_iter()
            .map(|(x, y)| Coord {
                x: ((me.x as isize) + x),
                y: ((me.y as isize) + y),
            })
            .filter(|c| self.in_bounds(c))
            .filter(|c| !self.visited[c.y as usize][c.x as usize])
            .filter(|c| is_valid_step(current, self[&c]))
            .collect();

        for coord in coords.iter() {
            self.visit(coord);
        }

        coords
    }

    /// Same neighbour function, except start at the end, and invert the logic so that we can go
    /// down by 1 height
    fn inverted_neighbours(&mut self, me: &Coord) -> Vec<Coord> {
        let from = self[me];

        let coords: Vec<Coord> = [(-1, 0), (1, 0), (0, -1), (0, 1)]
            .into_iter()
            .map(|(x, y)| Coord {
                x: ((me.x as isize) + x),
                y: ((me.y as isize) + y),
            })
            .filter(|c| self.in_bounds(c))
            .filter(|c| !self.visited[c.y as usize][c.x as usize])
            .filter(|to| is_valid_step(self[&to], from))
            .collect();

        for coord in coords.iter() {
            self.visit(coord);
        }

        coords
    }

    fn unvisit_all(&mut self) {
        self.visited = vec![vec![false; self.map[0].len()]; self.map.len()];
    }
}

fn is_valid_step(from: char, to: char) -> bool {
    let from = if from == 'E' { 'z' } else { from };
    let from = if from == 'S' { 'a' } else { from };

    let to = if to == 'S' { 'a' } else { to };
    let to = if to == 'E' { 'z' } else { to };

    (from as usize + 1) >= to as usize
}

impl Index<&Coord> for Grid {
    type Output = char;

    fn index(&self, index: &Coord) -> &Self::Output {
        &self.map[index.y as usize][index.x as usize]
    }
}

fn main() {
    let mut grid: Grid = stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap().chars().collect())
        .collect::<Vec<Vec<char>>>()
        .into();

    // instead of searching from start to end, we search from end to start
    // because the start can be either an 'a' or the starting point 'S'
    let start = grid.find_start();
    let end = grid.find_end();

    let mut queue: VecDeque<(usize, Coord)> = VecDeque::new();

    grid.visit(&start);
    queue.push_back((0, start));

    let steps: usize = loop {
        if let Some((steps, coord)) = queue.pop_front() {
            if grid[&coord] == 'E' {
                break steps;
            }

            for n in grid.new_neighbours(&coord) {
                queue.push_back((steps + 1, n));
            }
        } else {
            panic!("queue empty, no path found.. unreachable with correct input?");
        }
    };

    println!("min steps from S to E: {}", steps);

    grid.unvisit_all();
    grid.visit(&end);
    queue = VecDeque::new();
    queue.push_back((0, end));

    let steps: usize = loop {
        if let Some((steps, coord)) = queue.pop_front() {
            if grid[&coord] == 'a' {
                break steps;
            }

            for n in grid.inverted_neighbours(&coord) {
                queue.push_back((steps + 1, n));
            }
        } else {
            panic!("queue empty, no path found.. unreachable with correct input?");
        }
    };

    println!("min steps from a to E: {}", steps);
}
