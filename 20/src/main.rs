use std::io::{stdin, Read};

#[derive(Debug, PartialEq, Clone, Copy)]
enum Placement<T> {
    Moved(T),
    Init(T),
}

impl<T: Copy> Placement<T> {
    fn as_val(&self) -> T {
        match &self {
            Placement::Moved(val) => *val,
            Placement::Init(val) => *val,
        }
    }
}

fn find_grove(numbers: &Vec<(usize, Placement<i64>)>) -> i64 {
    let (zero, _) = numbers
        .iter()
        .enumerate()
        .find(|(_, (_, e))| e.as_val() == 0)
        .unwrap();

    println!("zero is at {}", zero);

    let p1 = numbers[(zero + 1000) % numbers.len()].1.as_val();
    let p2 = numbers[(zero + 2000) % numbers.len()].1.as_val();
    let p3 = numbers[(zero + 3000) % numbers.len()].1.as_val();

    println!("1000 contains {:?}", numbers[(zero + 1000) % numbers.len()]);
    println!("2000 contains {:?}", numbers[(zero + 2000) % numbers.len()]);
    println!("3000 contains {:?}", numbers[(zero + 3000) % numbers.len()]);

    let sum = p1 + p2 + p3;

    sum
}

fn reorder(numbers: &mut Vec<(usize, Placement<i64>)>) {
    let mut idx = 0;
    while idx < numbers.len() {
        if let (pos, Placement::Init(num)) = numbers[idx] {
            // println!("out: {:?}", numbers);
            numbers.remove(idx);
            let desired_pos = (idx as i64 + num).rem_euclid(numbers.len() as i64);
            numbers.insert(desired_pos as usize, (pos, Placement::Moved(num)));
        } else {
            idx += 1;
        }
    }
}

fn reorder_with_order(numbers: &mut Vec<(usize, Placement<i64>)>) {
    for i in 0..numbers.len() {
        // access the numbers in the correct order by looking for increasing index
        let idx = numbers
            .iter()
            .enumerate()
            .find(|(_, (n, _))| *n == i)
            .map(|(idx, _)| idx)
            .unwrap();

        if let (pos, Placement::Init(num)) = numbers.remove(idx) {
            let desired_pos = (idx as i64 + num).rem_euclid(numbers.len() as i64);
            numbers.insert(desired_pos as usize, (pos, Placement::Moved(num)));
        } else {
            panic!("pos {} (idx {}) is not init", i, idx);
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
        .enumerate()
        .collect::<Vec<_>>();

    let mut numbers2 = numbers
        .clone()
        .into_iter()
        .map(|n| Placement::Init(n * mult))
        .enumerate()
        .collect::<Vec<_>>();

    reorder(&mut numbers1);
    println!("part 1: {}", find_grove(&numbers1));

    for _ in 0..10 {
        reorder_with_order(&mut numbers2);

        for i in 0..numbers2.len() {
            let (p, n) = numbers2[i];

            numbers2[i] = (p, Placement::Init(n.as_val()));
        }
    }

    println!("part 2: {}", find_grove(&numbers2));
}
