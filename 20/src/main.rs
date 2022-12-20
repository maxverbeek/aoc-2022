use std::io::{stdin, Read};

#[derive(Debug, PartialEq)]
enum Placement {
    Moved(i64),
    Init(i64),
}

impl Placement {
    fn as_val(&self) -> i64 {
        match &self {
            Placement::Moved(val) => *val,
            Placement::Init(val) => *val,
        }
    }
}

fn find_grove(numbers: &Vec<Placement>) -> i64 {
    let (zero, _) = numbers
        .iter()
        .enumerate()
        .find(|(_, e)| e.as_val() == 0)
        .unwrap();

    println!("zero is at {}", zero);

    let p1 = numbers[(zero + 1000) % numbers.len()].as_val();
    let p2 = numbers[(zero + 2000) % numbers.len()].as_val();
    let p3 = numbers[(zero + 3000) % numbers.len()].as_val();

    println!("1000 contains {:?}", numbers[(zero + 1000) % numbers.len()]);
    println!("2000 contains {:?}", numbers[(zero + 2000) % numbers.len()]);
    println!("3000 contains {:?}", numbers[(zero + 3000) % numbers.len()]);

    let sum = p1 + p2 + p3;

    sum
}

fn reorder(numbers: &mut Vec<Placement>, times: i64) {
    let mut idx = 0;
    while idx < numbers.len() {
        if let Placement::Init(num) = numbers[idx] {
            // println!("out: {:?}", numbers);
            numbers.remove(idx);
            let desired_pos = (idx as i64 + num * times).rem_euclid(numbers.len() as i64);
            numbers.insert(desired_pos as usize, Placement::Moved(num));
        } else {
            idx += 1;
        }
    }
}

fn main() {
    let mut buf = String::new();
    stdin().lock().read_to_string(&mut buf).unwrap();

    let numbers = buf
        .split_whitespace()
        .map(|num| num.parse::<i64>().unwrap())
        .collect::<Vec<_>>();

    let mult = 811589153;

    let mut numbers1 = numbers
        .clone()
        .into_iter()
        .map(|n| Placement::Init(n))
        .collect::<Vec<_>>();

    let mut numbers2 = numbers
        .clone()
        .into_iter()
        .map(|n| Placement::Init(n * mult))
        .collect::<Vec<_>>();

    reorder(&mut numbers1, 1);

    reorder(&mut numbers2, 10);

    println!("part 1: {}", find_grove(&numbers1));
    println!("part 2: {}", find_grove(&numbers2));
}
