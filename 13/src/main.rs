use serde::Deserialize;
use std::{
    cmp::Ordering,
    io::{stdin, Read},
    str::FromStr,
};

#[derive(Debug, Eq, PartialEq, Deserialize, Ord, Clone)]
#[serde(untagged)]
enum Item {
    Number(i64),
    List(Vec<Item>),
}

impl Item {
    fn is_before(&self, rhs: &Item) -> bool {
        use Item::*;

        match (self, rhs) {
            (Number(lhs), Number(rhs)) => lhs < rhs,
            (List(lhs), List(rhs)) => compare_lists(lhs, rhs),
            (Number(lhs), rhs) => List(vec![Number(*lhs)]).is_before(rhs),
            (lhs, Number(rhs)) => lhs.is_before(&List(vec![Number(*rhs)])),
        }
    }
}

impl PartialOrd for Item {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.is_before(other) {
            Some(Ordering::Less)
        } else if other.is_before(&self) {
            Some(Ordering::Greater)
        } else {
            Some(Ordering::Equal)
        }
    }
}

fn compare_lists(lhs: &[Item], rhs: &[Item]) -> bool {
    for (l, r) in lhs.iter().zip(rhs.iter()) {
        if l.is_before(r) {
            return true;
        }

        if r.is_before(l) {
            return false;
        }
    }

    lhs.len() < rhs.len()
}

#[derive(Debug)]
struct Pair {
    first: Item,
    second: Item,
}

impl FromStr for Pair {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut items = s.lines().map(|l| serde_json::from_str::<Item>(l).unwrap());

        Ok(Pair {
            first: items.next().unwrap(),
            second: items.next().unwrap(),
        })
    }
}

fn main() {
    let input = {
        let mut input = String::new();
        stdin().lock().read_to_string(&mut input).unwrap();
        input
    };

    let pairs: Vec<Pair> = input
        .split("\n\n")
        .map(|p| p.parse::<Pair>().unwrap())
        .collect();

    let mut sum = 0;

    for (idx, p) in (1..).zip(pairs.iter()) {
        if p.first.is_before(&p.second) {
            sum += idx;
        }
    }

    println!("sum: {}", sum);

    let mut items: Vec<Item> = pairs
        .into_iter()
        .map(|p| [p.first, p.second].into_iter())
        .flatten()
        .collect();

    let marker2 = Item::List(vec![Item::Number(2)]);
    let marker6 = Item::List(vec![Item::Number(6)]);

    items.push(marker2.clone());
    items.push(marker6.clone());
    items.sort();

    let idx2 = items.iter().position(|i| *i == marker2).unwrap() + 1;
    let idx6 = items.iter().position(|i| *i == marker6).unwrap() + 1;

    println!("marker multiplication: {}", idx2 * idx6);
}

#[cfg(test)]
mod tests {
    use crate::Item::*;
    use crate::*;

    #[test]
    fn test_serde() {
        let inp = "[1,2,3,[4,5]]";

        assert_eq!(
            List(vec![
                Number(1),
                Number(2),
                Number(3),
                List(vec![Number(4), Number(5)])
            ]),
            serde_json::from_str::<Item>(inp).unwrap()
        );
    }

    #[test]
    fn test_stupid_edgecase() {
        let pair = r#"[[1],[2,3,4]]
[[1],4]"#
            .parse::<Pair>()
            .unwrap();

        assert!(pair.first.is_before(&pair.second));
    }

    #[test]
    fn test_stupid_edgecase2() {
        let pair = r#"[2,3,4]
[4]"#
            .parse::<Pair>()
            .unwrap();

        assert!(pair.first.is_before(&pair.second));
    }
}
