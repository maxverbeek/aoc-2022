use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    io::{stdin, BufRead},
};

use regex::Regex;

#[derive(Debug)]
struct Node {
    rate: usize,
    children: Vec<String>,
}

#[derive(Clone, Debug)]
struct Path {
    opened: HashSet<String>,
    flow: usize,
}

impl Path {
    fn should_replace(&self, name: &str, flow: usize) -> bool {
        (!self.opened.contains(name)) && self.flow <= flow
    }

    fn new() -> Self {
        Self {
            opened: HashSet::new(),
            flow: 0,
        }
    }

    fn copy_with(&self, node: String, flow: usize) -> Self {
        let mut ret = self.clone();
        if self.opened.contains(&node) {
            panic!("cannot add duplicate node {}, {:?}", node, self.opened);
        }
        ret.opened.insert(node.to_owned());
        ret.flow = flow;

        ret
    }
}

fn main() {
    let regex =
        Regex::new(r"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)").unwrap();

    let mut nodes = HashMap::new();

    for line in stdin().lock().lines().map(|l| l.unwrap()) {
        let captures = regex.captures(&line).ok_or("regex doesnt match").unwrap();

        let node: String = captures[1].into();
        let rate: usize = captures[2].parse().unwrap();
        let children: Vec<String> = captures[3].split(", ").map(|s| s.to_owned()).collect();

        nodes.insert(node, Node { rate, children });
    }

    let mut memo: Vec<HashMap<&str, Path>> = vec![HashMap::new(); 31];

    memo[0].insert("AA", Path::new());

    for t in 1..=30 {
        // opening the valve takes 1 minute, moving takes 1 minute.. so consider opening the
        // valve + moving from 2 minutes ago, or consider moving and not added the flow from 1
        // minute ago.
        for (&p, path) in &memo[t - 1].clone() {
            let previous = nodes.get(p).unwrap();

            for next in &previous.children {
                println!("after {} dont open {}, set its flow to {:?}", p, next, path);
                match memo[t].entry(next) {
                    Entry::Occupied(mut e) => {
                        if e.get().flow < path.flow {
                            e.insert(path.clone());
                        }
                    }
                    Entry::Vacant(e) => {
                        e.insert(path.clone());
                    }
                };
            }
        }

        if t < 2 {
            continue;
        }

        // let remaining = if t < 30 { 29 - t } else { 0 };
        let remaining = 30 - (t - 2);

        for (pname, path) in &memo[t - 2].clone() {
            let previous = nodes.get(*pname).unwrap();

            for next in &previous.children {
                let newflow = path.flow + previous.rate * remaining;

                let newpath = if path.should_replace(pname, newflow) {
                    path.copy_with(pname.to_string(), newflow)
                } else {
                    path.clone()
                };

                println!(
                    "after {} open {}, set its flow to {:?}",
                    pname, next, newpath
                );

                match memo[t].entry(next) {
                    Entry::Occupied(mut e) => {
                        if e.get().flow < newflow {
                            e.insert(newpath);
                        }
                    }
                    Entry::Vacant(e) => {
                        e.insert(newpath);
                    }
                };
            }
        }
    }

    for (name, path) in &memo[30] {
        println!("{}: {}", name, path.flow);
    }
}
