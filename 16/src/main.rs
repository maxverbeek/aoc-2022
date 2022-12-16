use std::{
    borrow::Borrow,
    collections::HashMap,
    io::{stdin, BufRead},
};

use regex::Regex;

#[derive(Debug)]
struct Node {
    rate: usize,
    children: Vec<String>,
}

fn main() {
    let regex =
        Regex::new(r"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)").unwrap();

    let mut nodes: HashMap<String, Node> = HashMap::new();

    for line in stdin().lock().lines().map(|l| l.unwrap()) {
        let captures = regex.captures(&line).ok_or("regex doesnt match").unwrap();

        let node: String = captures[1].into();
        let rate: usize = captures[2].parse().unwrap();
        let children: Vec<String> = captures[3].split(", ").map(|s| s.to_owned()).collect();

        nodes.insert(node, Node { rate, children });
    }

    let mut memo: Vec<HashMap<&str, usize>> = vec![HashMap::new(); 31];

    memo[0].insert("AA", 0);

    for t in 1..=30 {
        let remaining = 30 - t;

        for neighbour in memo[t - 1]
            .iter()
            .map(|(name, _e)| nodes.get(*name).expect("node exists").children.iter())
            .flatten()
        {
            let node = nodes.get(neighbour).unwrap();
            println!("{:?}", node);
        }
    }
}
