use std::{
    error::Error,
    io::{stdin, BufRead},
    str::FromStr,
};

enum Instruction {
    Noop,
    AddX(i32),
}

impl Instruction {
    fn cycle_length(&self) -> i32 {
        match &self {
            Self::Noop => 1,
            Self::AddX(_) => 2,
        }
    }
}

impl FromStr for Instruction {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let words: Vec<&str> = s.split(" ").collect();
        match words[0] {
            "noop" => return Ok(Self::Noop),
            "addx" => return Ok(Self::AddX(words[1].parse()?)),
            _ => Err("wrong instruction".into()),
        }
    }
}

fn count_signal(cycle: &i32, x: &i32) -> i32 {
    if (cycle - 20) % 40 == 0 && (cycle - 20) / 40 <= 5 && cycle > &0 {
        return cycle * x;
    }

    return 0;
}

fn print_char(cycle: &i32, x: &i32) {
    let col = (cycle - 1) % 40;

    if (x - col).abs() <= 1 {
        print!("#");
    } else {
        print!(".");
    }

    if col == 39 {
        print!("\n");
    }
}

fn main() {
    let instructions: Vec<Instruction> = stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap().parse().unwrap())
        .collect();

    let mut cycle = 1;
    let mut x = 1;

    let mut signalsum = 0;

    for instruction in instructions {
        for _ in 0..instruction.cycle_length() {
            signalsum += count_signal(&cycle, &x);
            print_char(&cycle, &x);
            cycle += 1;
        }

        match instruction {
            Instruction::Noop => {}
            Instruction::AddX(addx) => x += addx,
        }
    }

    println!("signal sum {}", signalsum);
}
