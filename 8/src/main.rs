use std::io::{stdin, BufRead};

struct Tree {
    height: usize,
    visible: bool,
    scenic: i32,
}

impl From<char> for Tree {
    fn from(c: char) -> Self {
        Self {
            height: c.to_digit(10).unwrap() as usize,
            visible: false,
            scenic: 1,
        }
    }
}

fn topcheck(tops: &mut [i32], tree: &mut Tree, location: usize) {
    // find the scenic multiplier by finding out the location of the tree with the same height
    // in previous iterations
    let intersect = tops[tree.height];

    if intersect == -1 {
        tree.visible = true;
        tree.scenic *= location as i32;
    } else {
        tree.scenic *= (location as i32) - (intersect as i32);
    }

    // any tree that is not higher than this one will look at this one
    for h in 0..=tree.height {
        tops[h] = location as i32;
    }
}

fn main() {
    let mut lines = stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap().chars().map(|c| c.into()).collect::<Vec<Tree>>())
        .collect::<Vec<Vec<Tree>>>();

    let w = lines[0].len();
    let h = lines.len();

    // for all rows:
    for y in 0..h {
        // east to west
        let mut tops = [-1; 10];
        for x in 0..w {
            topcheck(&mut tops, &mut lines[y][x], x);
        }

        // west to east
        let mut tops = [-1; 10];
        for x in (0..w).rev() {
            topcheck(&mut tops, &mut lines[y][x], w - x - 1);
        }
    }

    // for all cols
    for x in 0..w {
        // north to south
        let mut tops = [-1; 10];
        for y in 0..h {
            topcheck(&mut tops, &mut lines[y][x], y);
        }

        // south to north
        let mut tops = [-1; 10];
        for y in (0..h).rev() {
            topcheck(&mut tops, &mut lines[y][x], h - y - 1);
        }
    }

    let mut count = 0;

    for line in lines.iter() {
        for tree in line {
            if tree.visible {
                count += 1;
            } else {
            }
            print!("{:01$} ", tree.scenic, 2);
        }
        print!("\n");
    }

    println!("visible trees: {}", count);

    let scenic = lines
        .iter()
        .map(|l| l.iter().map(|t| t.scenic).max().unwrap())
        .max()
        .unwrap();

    println!("highest scenic score: {}", scenic);
}
