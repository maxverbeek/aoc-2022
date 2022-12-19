use std::{
    collections::HashMap,
    io::{stdin, Read},
    sync::{Arc, Mutex},
    thread,
};

mod bottomup;
mod naivememo;
mod types;

use types::Blueprint;

fn main() {
    let mut input = String::new();
    stdin().lock().read_to_string(&mut input).unwrap();

    let blueprints: Vec<Blueprint> = input
        .trim()
        .split("\n")
        .map(|bp| bp.parse().unwrap())
        .collect();

    let idxsum = Arc::new(Mutex::new(0));

    let mut handles = vec![];

    for bp in &blueprints {
        let idxsum = idxsum.clone();
        let bp = bp.clone();
        let handle = thread::spawn(move || {
            let mut memo = HashMap::new();
            let geodes = naivememo::find_geodes(&bp, &mut memo, 24, 0, 0, 0, 1, 0, 0, 0);

            println!("bp {} produces {} geodes", bp.idx, geodes);

            let mut idxsum = idxsum.lock().unwrap();
            *idxsum += bp.idx * geodes;
        });

        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("total quality level: {}", idxsum.lock().unwrap());

    println!("--- elephant found food and ate nearly all blueprints ---");

    let part2sum = Arc::new(Mutex::new(0));

    for bp in &blueprints[0..2] {
        let part2sum = part2sum.clone();
        let bp = bp.clone();
        let mut memo = HashMap::new();
        let geodes = naivememo::find_geodes(&bp, &mut memo, 32, 0, 0, 0, 1, 0, 0, 0);

        println!("bp {} produces {} geodes", bp.idx, geodes);

        let mut part2sum = part2sum.lock().unwrap();
        *part2sum += bp.idx * geodes;
    }

    println!("part 2 quality level: {}", part2sum.lock().unwrap());
}
