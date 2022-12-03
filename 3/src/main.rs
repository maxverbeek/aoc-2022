use std::{
    collections::HashSet,
    io::{stdin, BufRead},
};

fn main() {
    part2();
}

fn part1() {
    let lines = stdin().lock().lines().map(|l| l.unwrap().to_string());
    let mut score: u32 = 0;

    for line in lines {
        let (fst, last) = line.split_at(line.len() / 2);

        let mut hashset = HashSet::new();

        for c in fst.chars() {
            hashset.insert(c);
        }

        for c in last.chars() {
            if hashset.get(&c).is_some() {
                score += get_prio(&c) as u32;
                hashset.remove(&c);
            }
        }
    }

    println!("score: {}", score)
}

fn part2() {
    let mut score = 0 as u32;
    let lines: Vec<String> = stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap().to_string())
        .collect();

    for chunk in lines.chunks(3) {
        let unique = chunk
            .into_iter()
            .map(|c| -> HashSet<char> { c.to_owned().chars().into_iter().collect() })
            .reduce(|mut c, n| {
                c.retain(|f| n.contains(f));
                c
            })
            .expect("has value");

        score += get_prio(&unique.into_iter().next().expect("has at at least one")) as u32;
    }

    println!("{}", score);
}

fn get_prio(c: &char) -> u8 {
    if !c.is_ascii() {
        panic!("must be ascii input");
    }

    if c.is_uppercase() {
        (*c as u8) - 'A' as u8 + 27
    } else {
        (*c as u8) - 'a' as u8 + 1
    }
}
