use std::io::{stdin, BufRead};

#[derive(Debug)]
struct Crates {
    crates: Vec<Vec<char>>,
}

impl Crates {
    fn move_crate(&mut self, quantity: usize, from: usize, to: usize) {
        for _ in 0..quantity {
            let thing = self.crates[from]
                .pop()
                .unwrap_or_else(|| panic!("{} is already empty", from));

            self.crates[to].push(thing);
        }
    }

    fn move_crate_inorder(&mut self, quantity: usize, from: usize, to: usize) {
        let endpos = self.crates[from].len() - quantity;
        let mut things = self.crates[from].split_off(endpos);
        self.crates[to].append(&mut things);
    }

    fn get_tops(&self) -> String {
        self.crates
            .iter()
            .filter_map(|c| c.last())
            .collect::<String>()
    }
}

impl From<Vec<String>> for Crates {
    fn from(mut lines: Vec<String>) -> Self {
        let numcrates = lines
            .pop()
            .expect("has last line")
            .split(' ')
            .filter(|x| *x != "")
            .map(|num| num.trim())
            .map(|digit| digit.parse::<usize>().expect("num is parsable"))
            .last()
            .expect("has last digit");

        let mut crates = Crates {
            crates: vec![vec![]; numcrates],
        };

        // [B] [W] [N] [P] [D] [V] [G] [L] [T]
        for line in lines {
            for (pos, c) in line.char_indices().skip(1).step_by(4) {
                if c == ' ' {
                    continue;
                }

                let idx = (pos - 1) / 4;
                crates.crates[idx].insert(0, c);
            }
        }

        crates
    }
}

fn parse_line(line: String) -> (usize, usize, usize) {
    // move x from x to x
    let components = line
        .split(" ")
        .filter_map(|f| f.parse::<usize>().ok())
        .collect::<Vec<usize>>();

    // subtract 1 from `from` and `to` to represent a 0-based index
    (components[0], components[1] - 1, components[2] - 1)
}

fn main() {
    let mut initial_config: Crates = stdin()
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .take_while(|x| x != "")
        .collect::<Vec<String>>()
        .into();

    for line in stdin().lock().lines() {
        let (q, f, t) = parse_line(line.unwrap());
        // part 1:
        // initial_config.move_crate(q, f, t);

        // part 2:
        initial_config.move_crate_inorder(q, f, t);
    }

    println!("{}", initial_config.get_tops());
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse() {
        let result = parse_line("move 2 from 1 to 4".to_owned());
        assert_eq!(result, (2, 0, 3));
    }
}
