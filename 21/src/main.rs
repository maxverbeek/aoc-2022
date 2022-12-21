use std::{
    collections::HashMap,
    io::{stdin, BufRead},
    str::FromStr,
};

#[derive(Debug, Clone)]
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

    fn op(&self) -> &'static str {
        match self {
            Expr::Num(_) => "id",
            Expr::Add(_, _) => "+",
            Expr::Sub(_, _) => "-",
            Expr::Mul(_, _) => "*",
            Expr::Div(_, _) => "/",
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

fn find_human(monkeys: &HashMap<String, Expr>, key: &str) -> LinearFunc {
    if key == "humn" {
        return LinearFunc {
            slope: 1.0,
            intercept: 0.0,
        };
    }

    let expr = monkeys.get(key).unwrap();

    if let Expr::Num(n) = expr {
        return LinearFunc {
            slope: 0.0,
            intercept: *n as f64,
        };
    } else {
        let (k1, k2) = expr.pairs();

        let mut e1 = find_human(monkeys, k1);

        if e1.slope != 0.0 {
            // e1 has human component
            let rhs = eval(monkeys, k2);

            match expr {
                Expr::Num(_) => unreachable!(),
                Expr::Add(_, _) => e1.add(rhs),
                Expr::Sub(_, _) => e1.sub(rhs),
                Expr::Mul(_, _) => e1.scale(rhs),
                Expr::Div(_, _) => e1.div(rhs),
            }

            return e1;
        }

        let mut e2 = find_human(monkeys, k2);

        if e2.slope != 0.0 {
            // e2 has human component
            let lhs = eval(monkeys, k1);

            match expr {
                Expr::Num(_) => unreachable!(),
                Expr::Add(_, _) => e2.add(lhs),
                Expr::Sub(_, _) => {
                    // lhs - x => -(-lhs + x) => -(x - lhs)
                    e2.sub(lhs);
                    e2.scale(-1);
                }
                Expr::Mul(_, _) => e2.scale(lhs),
                Expr::Div(_, _) => unimplemented!("dividing by something with a slope is hard"),
            }

            return e2;
        }

        LinearFunc {
            slope: 0f64,
            intercept: eval(monkeys, key) as f64,
        }
    }
}

#[derive(Debug)]
struct LinearFunc {
    slope: f64,
    intercept: f64,
}

impl LinearFunc {
    fn add(&mut self, x: i64) {
        self.intercept += x as f64;
    }

    fn scale(&mut self, x: i64) {
        self.intercept *= x as f64;
        self.slope *= x as f64;
    }

    fn sub(&mut self, x: i64) {
        self.intercept -= x as f64;
    }

    fn div(&mut self, x: i64) {
        self.intercept /= x as f64;
        self.slope /= x as f64;
    }

    fn solve(&self, rhs: &Self) -> f64 {
        if self.slope != 0.0 {
            (rhs.intercept - self.intercept) / self.slope
        } else {
            (self.intercept - rhs.intercept) / rhs.slope
        }
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

    let (k1, k2) = &monkeys.get("root").unwrap().pairs();

    let lhs = find_human(&monkeys, k1);
    let rhs = find_human(&monkeys, k2);

    let ans = lhs.solve(&rhs) as i64;

    println!("answer for human: {}", ans);
}
