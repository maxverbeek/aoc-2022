use std::{
    collections::HashMap,
    io::{stdin, BufRead},
};

use regex::Regex;

#[derive(Debug)]
struct Node {
    rate: i32,
    children: Vec<String>,
    idx: usize,
}

fn solve(
    node: &str,
    nodes: &HashMap<String, Node>,
    opened: &mut Vec<String>,
    steps_remain: i32,
    memo: &mut HashMap<(String, Vec<String>, i32), i32>,
) -> i32 {
    if steps_remain <= 0 {
        return 0;
    }

    let key = (node.to_owned(), opened.clone(), steps_remain);

    if let Some(&res) = memo.get(&key) {
        return res;
    }

    let mut best = 0;
    let curnode = nodes.get(node).unwrap();

    if curnode.rate > 0 && !opened.contains(&node.to_string()) {
        // flow in this node when opened
        let flow = nodes.get(node).unwrap().rate * (steps_remain - 1);
        opened.push(node.to_string());

        for child in &curnode.children {
            let next = solve(&child, nodes, opened, steps_remain - 2, memo);
            best = best.max(flow + next);
        }

        opened.pop();
    }

    // do not open the node here and go straight to the next one
    for child in &curnode.children {
        let next = solve(&child, nodes, opened, steps_remain - 1, memo);
        best = best.max(next);
    }

    memo.insert(key, best);

    best
}

type OpenMask = usize;

trait Set {
    fn is_open(&self, idx: usize) -> bool;
    fn set_open(&mut self, idx: usize, count: &mut usize);
    fn set_closed(&mut self, idx: usize, count: &mut usize);
}

impl Set for OpenMask {
    fn is_open(&self, idx: usize) -> bool {
        (*self) & 1 << idx > 0
    }

    fn set_open(&mut self, idx: usize, count: &mut usize) {
        *self = (*self) | 1 << idx;
        *count += 1;
    }

    fn set_closed(&mut self, idx: usize, count: &mut usize) {
        *self = (*self) & !(1 << idx);
        *count -= 1;
    }
}

fn solve_part2(
    myloc: &str,
    elephantloc: &str,
    nodes: &HashMap<String, Node>,
    opened: &mut OpenMask,
    steps_remain: i32,
    memo: &mut HashMap<(String, String, usize, i32), i32>,
    total_nodes: usize,
    open_count: usize,
) -> i32 {
    if steps_remain <= 0 {
        return 0;
    }

    if open_count == total_nodes {
        return 0;
    }

    // declare open_count as mutable
    let mut open_count = open_count;

    let key = (
        myloc.to_owned(),
        elephantloc.to_owned(),
        *opened,
        steps_remain,
    );

    if let Some(&res) = memo.get(&key) {
        return res;
    }

    let mut best = 0;

    let mynode = nodes.get(myloc).unwrap();
    let elnode = nodes.get(elephantloc).unwrap();

    // attempt to open the valve where i am currently at
    if mynode.rate > 0 && !opened.is_open(mynode.idx) {
        // flow in this node when opened
        let myflow = mynode.rate * (steps_remain - 1);
        opened.set_open(mynode.idx, &mut open_count);

        for mychild in &mynode.children {
            // with my valve opened, have the elephant perform a similar routine
            if elnode.rate > 0 && !opened.is_open(elnode.idx) {
                let elflow = elnode.rate * (steps_remain - 1);
                opened.set_open(elnode.idx, &mut open_count);

                for elchild in &elnode.children {
                    let next = myflow
                        + elflow
                        + solve_part2(
                            mychild,
                            elchild,
                            nodes,
                            opened,
                            steps_remain - 2,
                            memo,
                            total_nodes,
                            open_count,
                        );

                    best = best.max(next);
                }

                opened.set_closed(elnode.idx, &mut open_count);
            }
        }

        // i am opening a valve but the elephant isnt
        for elchild in &elnode.children {
            let next = myflow
                + solve_part2(
                    myloc,
                    elchild,
                    nodes,
                    opened,
                    steps_remain - 1,
                    memo,
                    total_nodes,
                    open_count,
                );
            best = best.max(next);
        }

        opened.set_closed(mynode.idx, &mut open_count);
    }

    // do not open the node here and go straight to the next one
    // instead the elephant can open a valve (maybe)
    if elnode.rate > 0 && !opened.is_open(elnode.idx) {
        let elflow = elnode.rate * (steps_remain - 1);
        opened.set_open(elnode.idx, &mut open_count);

        for mychild in &mynode.children {
            let next = solve_part2(
                mychild,
                elephantloc,
                nodes,
                opened,
                steps_remain - 1,
                memo,
                total_nodes,
                open_count,
            );
            best = best.max(next + elflow);
        }

        opened.set_closed(elnode.idx, &mut open_count);
    }

    // neither of us open a node
    for mychild in &mynode.children {
        for elchild in &elnode.children {
            let next = solve_part2(
                mychild,
                elchild,
                nodes,
                opened,
                steps_remain - 1,
                memo,
                total_nodes,
                open_count,
            );
            best = best.max(next);
        }
    }

    memo.insert(key, best);

    best
}

fn main() {
    let regex =
        Regex::new(r"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)").unwrap();

    let mut nodes = HashMap::new();

    for line in stdin().lock().lines().map(|l| l.unwrap()) {
        let captures = regex.captures(&line).ok_or("regex doesnt match").unwrap();

        let node: String = captures[1].into();
        let rate: i32 = captures[2].parse().unwrap();
        let children: Vec<String> = captures[3].split(", ").map(|s| s.to_owned()).collect();

        nodes.insert(
            node,
            Node {
                rate,
                children,
                idx: 0,
            },
        );
    }

    let mut idx = 0;

    for (_key, node) in nodes.iter_mut() {
        node.idx = idx;
        idx += 1;
    }

    let mut memo = HashMap::new();
    let mut opened: Vec<String> = vec![];

    let part1 = solve("AA", &nodes, &mut opened, 30, &mut memo);

    println!("part 1: {}", part1);

    let mut memo = HashMap::new();
    let mut opened: OpenMask = 0;

    let non_zero = nodes.iter().filter(|(_, n)| n.rate > 0).count();

    let part2 = solve_part2("AA", "AA", &nodes, &mut opened, 26, &mut memo, non_zero, 0);

    println!("part 2: {}", part2);
}
