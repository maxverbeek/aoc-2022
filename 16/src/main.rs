use std::{
    collections::{HashMap, HashSet},
    io::{stdin, BufRead},
};

use regex::Regex;

#[derive(Debug)]
struct Node {
    rate: i32,
    children: Vec<String>,
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

fn solve_part2(
    myloc: &str,
    elephantloc: &str,
    nodes: &HashMap<String, Node>,
    opened: &mut Vec<String>,
    steps_remain: i32,
    memo: &mut HashMap<(String, String, Vec<String>, i32), i32>,
    total_nodes: usize,
) -> i32 {
    if steps_remain <= 0 {
        return 0;
    }

    if opened.len() == total_nodes {
        return 0;
    }

    let key = (
        myloc.to_owned(),
        elephantloc.to_owned(),
        opened.clone(),
        steps_remain,
    );

    if let Some(&res) = memo.get(&key) {
        return res;
    }

    let mut best = 0;

    let mynode = nodes.get(myloc).unwrap();
    let elnode = nodes.get(elephantloc).unwrap();

    // attempt to open the valve where i am currently at
    if mynode.rate > 0 && !opened.contains(&myloc.to_string()) {
        // flow in this node when opened
        let myflow = mynode.rate * (steps_remain - 1);
        opened.push(myloc.to_string());

        for mychild in &mynode.children {
            // with my valve opened, have the elephant perform a similar routine
            if elnode.rate > 0 && !opened.contains(&elephantloc.to_string()) {
                let elflow = elnode.rate * (steps_remain - 1);
                opened.push(elephantloc.to_string());

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
                        );

                    best = best.max(next);
                }

                opened.pop();
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
                );
            best = best.max(next);
        }

        opened.pop();
    }

    // do not open the node here and go straight to the next one
    // instead the elephant can open a valve (maybe)
    if elnode.rate > 0 && !opened.contains(&elephantloc.to_string()) {
        let elflow = elnode.rate * (steps_remain - 1);
        opened.push(elephantloc.to_string());

        for mychild in &mynode.children {
            let next = solve_part2(
                mychild,
                elephantloc,
                nodes,
                opened,
                steps_remain - 1,
                memo,
                total_nodes,
            );
            best = best.max(next + elflow);
        }

        opened.pop();
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

        nodes.insert(node, Node { rate, children });
    }

    let mut memo = HashMap::new();
    let mut opened: Vec<String> = vec![];

    let part1 = solve("AA", &nodes, &mut opened, 30, &mut memo);

    println!("part 1: {}", part1);

    let mut memo = HashMap::new();
    let mut opened: Vec<String> = vec![];

    let non_zero = nodes.iter().filter(|(_, n)| n.rate > 0).count();

    let part2 = solve_part2("AA", "AA", &nodes, &mut opened, 26, &mut memo, non_zero);

    println!("part 2: {}", part2);
}
