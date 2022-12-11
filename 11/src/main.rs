use std::{
    cmp::Reverse,
    collections::VecDeque,
    error::Error,
    io::{stdin, Read},
    str::FromStr,
};

struct Monkey {
    inspect_count: i64,
    items: VecDeque<i64>,
    operation: Box<dyn Operation>,
    test: Test,
}

impl Monkey {
    fn inspect_first(&mut self) -> Option<i64> {
        if let Some(item) = self.items.pop_front() {
            self.inspect_count += 1;
            return Some(self.operation.perform(item));
        }

        return None;
    }
}

impl FromStr for Monkey {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();

        // first line is the monkey definition
        let _monkey = lines.next().unwrap();

        // second line is the starting item list: extract all integers
        let starting: VecDeque<_> = lines
            .next()
            .unwrap()
            .trim()
            .split(|c| [' ', ','].contains(&c))
            .filter_map(|word| word.parse().ok())
            .collect();

        // third line is the Operation line
        // Operation: new = old + 6
        let operation: Box<dyn Operation> = match lines
            .next()
            .unwrap()
            .trim()
            .split(" ")
            .collect::<Vec<&str>>()
            .as_slice()
        {
            ["Operation:", "new", "=", "old", "+", "old"] => Box::new(OpMul { operand: 2 }), // double
            ["Operation:", "new", "=", "old", "+", operand] => Box::new(OpAdd {
                operand: operand.parse::<i64>()?,
            }),
            ["Operation:", "new", "=", "old", "*", "old"] => Box::new(OpSquare {}),
            ["Operation:", "new", "=", "old", "*", operand] => Box::new(OpMul {
                operand: operand.parse::<i64>()?,
            }),
            _ => return Err("wrong operation statement".into()),
        };

        // next 3 lines are the test lines
        let teststatement = lines.collect::<String>();

        let mut test_nums = teststatement
            .split_whitespace() // split to words
            .filter_map(|w| w.parse::<i64>().ok()); // take all words that are numeric

        let test = Test::new(
            test_nums.next().unwrap(),
            test_nums.next().unwrap() as usize,
            test_nums.next().unwrap() as usize,
        );

        Ok(Monkey {
            inspect_count: 0,
            items: starting,
            operation,
            test,
        })
    }
}

struct OpAdd {
    operand: i64,
}

struct OpMul {
    operand: i64,
}

struct OpSquare;

trait Operation {
    fn perform(&self, input: i64) -> i64;
}

impl Operation for OpAdd {
    fn perform(&self, input: i64) -> i64 {
        self.operand + input
    }
}

impl Operation for OpMul {
    fn perform(&self, input: i64) -> i64 {
        self.operand * input
    }
}

impl Operation for OpSquare {
    fn perform(&self, input: i64) -> i64 {
        input * input
    }
}

#[derive(Debug)]
struct Test {
    divider: i64,
    res_true: usize,
    res_false: usize,
}

impl Test {
    fn new(div: i64, t: usize, f: usize) -> Self {
        Self {
            divider: div,
            res_true: t,
            res_false: f,
        }
    }

    fn get_destination(&self, worry_level: i64) -> usize {
        if worry_level % self.divider == 0 {
            self.res_true
        } else {
            self.res_false
        }
    }
}

fn solve(input: &str, part: usize) {
    let mut monkeys: Vec<Monkey> = input.split("\n\n").map(|m| m.parse().unwrap()).collect();

    let divspace: i64 = monkeys.iter().map(|m| m.test.divider).product();

    let rounds = if part == 1 { 20 } else { 10_000 };

    for round in 1..=rounds {
        // println!("round {}", round);
        for mi in 0..monkeys.len() {
            while let Some(item) = monkeys[mi].inspect_first() {
                let item = if part == 1 { item / 3 } else { item % divspace };
                let dest = monkeys[mi].test.get_destination(item);
                monkeys[dest].items.push_back(item);
            }
        }

        for (i, m) in monkeys.iter().enumerate() {
            // println!("monkey {}: {:?}", i, m.items);
        }
    }

    let mut inspect_counts = monkeys.iter().map(|m| m.inspect_count).collect::<Vec<_>>();
    inspect_counts.sort_by_key(|w| Reverse(*w));

    let monkey_business: i64 = inspect_counts.iter().take(2).product();

    println!("monkey business part {}: {}", part, monkey_business);
}

fn main() {
    let mut input = String::new();
    stdin().lock().read_to_string(&mut input).unwrap();

    solve(&input, 1);
    solve(&input, 2);
}
