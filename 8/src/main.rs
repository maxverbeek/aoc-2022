use std::io::{stdin, BufRead};

struct Tree {
    c: char,
    height: u32,
    visible: bool,
}

impl From<char> for Tree {
    fn from(c: char) -> Self {
        Self {
            c,
            height: c.to_digit(10).unwrap(),
            visible: true,
        }
    }
}

fn topcheck(curtop: &mut u32, height: u32) -> bool {
    if height > *curtop {
        *curtop = height;
        return true;
    }

    return false;
}

fn main() {
    let mut lines = stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap().chars().map(|c| c.into()).collect::<Vec<Tree>>())
        .collect::<Vec<Vec<Tree>>>();

    let w = lines[0].len() - 1;
    let h = lines[1].len() - 1;

    for y in 0..=h {
        let mut top = 0;
        for x in 0..=w {
            lines[y][x].visible |= topcheck(&mut top, lines[y][x].height);
        }
    }

    for y in 0..=h {
        let mut top = 0;
        for x in w..=0 {
            lines[y][x].visible |= topcheck(&mut top, lines[y][x].height);
        }
    }

    for y in h..=0 {
        let mut top = 0;
        for x in 0..=w {
            lines[y][x].visible |= topcheck(&mut top, lines[y][x].height);
        }
    }

    for y in 0..=h {
        let mut top = 0;
        for x in w..=0 {
            lines[y][x].visible |= topcheck(&mut top, lines[y][x].height);
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
