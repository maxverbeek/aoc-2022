use std::io::{stdin, BufRead};

struct Tree {
    height: i32,
    visible: bool,
}

impl From<char> for Tree {
    fn from(c: char) -> Self {
        Self {
            height: c.to_digit(10).unwrap() as i32,
            visible: false,
        }
    }
}

fn topcheck(curtop: &mut i32, tree: &mut Tree) {
    if tree.height > *curtop {
        tree.visible = true;
        *curtop = tree.height;
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
        let mut top = -1;
        for x in 0..w {
            topcheck(&mut top, &mut lines[y][x]);
        }

        // west to east
        let mut top = -1;
        for x in (0..w).rev() {
            topcheck(&mut top, &mut lines[y][x]);
        }
    }

    // for all cols
    for x in 0..w {
        // north to south
        let mut top = -1;
        for y in 0..h {
            topcheck(&mut top, &mut lines[y][x]);
        }

        // south to north
        let mut top = -1;
        for y in (0..h).rev() {
            topcheck(&mut top, &mut lines[y][x]);
        }
    }

    let mut count = 0;

    for line in lines {
        for tree in line {
            if tree.visible {
                count += 1;
                print!("X");
            } else {
                print!(".");
            }
        }
        print!("\n");
    }

    println!("visible trees: {}", count);
}
