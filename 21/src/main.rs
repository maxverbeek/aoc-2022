use std::{
    collections::HashMap,
    io::{stdin, BufRead},
    str::FromStr,
};

#[derive(Debug)]
enum Expr {
    Num(i64),
    Add(String, String),
    Sub(String, String),
    Mul(String, String),
    Div(String, String),
}

impl Expr {
    fn pairs(&self) -> (&str, &str) {
        match &self {
            Expr::Num(_) => panic!("no lhs or rhs"),
            Expr::Add(a, b) => (&a, &b),
            Expr::Sub(a, b) => (&a, &b),
            Expr::Mul(a, b) => (&a, &b),
            Expr::Div(a, b) => (&a, &b),
        }
    }
}

impl FromStr for Expr {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let words = s.split(" ").collect::<Vec<&str>>();

        if words.len() == 1 {
            return Ok(Expr::Num(words[0].parse::<i64>().unwrap()));
        }

        match words.as_slice() {
            [a, "+", b] => Ok(Expr::Add(a.to_string(), b.to_string())),
            [a, "-", b] => Ok(Expr::Sub(a.to_string(), b.to_string())),
            [a, "*", b] => Ok(Expr::Mul(a.to_string(), b.to_string())),
            [a, "/", b] => Ok(Expr::Div(a.to_string(), b.to_string())),
            _ => Err("wrong pattern".into()),
        }
    }
}

fn eval(monkeys: &HashMap<String, Expr>, key: &str) -> i64 {
    if let Some(e) = monkeys.get(key) {
        match e {
            Expr::Num(n) => *n,
            Expr::Add(a, b) => eval(monkeys, a) + eval(monkeys, b),
            Expr::Sub(a, b) => eval(monkeys, a) - eval(monkeys, b),
            Expr::Mul(a, b) => eval(monkeys, a) * eval(monkeys, b),
            Expr::Div(a, b) => eval(monkeys, a) / eval(monkeys, b),
        }
    } else {
        panic!("no pattern in map: {}", key);
    }
}

fn main() {
    let mut monkeys = HashMap::new();

    for line in stdin().lock().lines().map(|l| l.unwrap()) {
        let mut split = line.split(": ");

        let monkey = split.next().unwrap();
        let expr = split.next().unwrap().parse::<Expr>().unwrap();

        monkeys.insert(monkey.to_string(), expr);
    }

    let res = eval(&monkeys, "root");

    println!("result: {}", res);
}
