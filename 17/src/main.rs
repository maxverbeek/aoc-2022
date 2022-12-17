use std::{
    collections::{hash_map::Entry, HashMap},
    io::{stdin, BufRead},
};

struct Rock {
    idx: usize,
    shape: Vec<(usize, usize)>,
    width: usize,
    height: usize,
}

fn rocks() -> Vec<Rock> {
    let hbar = Rock {
        idx: 0,
        shape: vec![(0, 0), (1, 0), (2, 0), (3, 0)],
        width: 4,
        height: 1,
    };

    let plus = Rock {
        idx: 1,
        shape: vec![(1, 0), (0, 1), (2, 1), (1, 2)],
        width: 3,
        height: 3,
    };

    let lshape = Rock {
        idx: 2,
        shape: vec![(2, 0), (2, 1), (2, 2), (1, 2), (0, 2)],
        width: 3,
        height: 3,
    };

    let vbar = Rock {
        idx: 3,
        shape: vec![(0, 0), (0, 1), (0, 2), (0, 3)],
        width: 1,
        height: 4,
    };

    let square = Rock {
        idx: 4,
        shape: vec![(0, 0), (0, 1), (1, 0), (1, 1)],
        width: 2,
        height: 2,
    };

    vec![hbar, plus, lshape, vbar, square]
}

enum Jet {
    L,
    R,
}

impl From<char> for Jet {
    fn from(c: char) -> Self {
        if c == '<' {
            Self::L
        } else if c == '>' {
            Self::R
        } else {
            panic!("bad char {}", c);
        }
    }
}

fn intersect(rock: &Rock, position: (usize, usize), map: &Vec<Vec<bool>>) -> bool {
    let (x, y) = position;

    for offset in &rock.shape {
        let x = x + offset.0;
        let y = y - offset.1;

        if map[y][x] {
            return true;
        }
    }

    false
}

fn main() {
    let templates = rocks();
    let jetmoves: Vec<Jet> = stdin()
        .lock()
        .lines()
        .next()
        .unwrap()
        .map(|l| l.chars().map(|c| c.into()).collect::<Vec<_>>())
        .unwrap();

    let cycle = (jetmoves.len() * templates.len()).max(4000);

    let mut map: Vec<Vec<bool>> = vec![vec![false; 7]; cycle * 50];

    let rocks = templates.iter().cycle();
    let mut jets = jetmoves.iter().enumerate().cycle();
    let mut top = 0;

    let mut states: HashMap<State, (usize, usize)> = HashMap::new();

    let mut rockidx: usize = 0;
    let mut jetidx: usize;

    let mut multiplied_added: usize = 0;

    let target: usize = 1_000_000_000_000;

    for rock in rocks {
        let mut x = 2;
        let mut y = top + rock.height + 2;

        loop {
            // move the block left or right
            let (ji, jet) = jets.next().unwrap();

            jetidx = ji;

            match jet {
                &Jet::L => {
                    // calculate any intersections
                    if x > 0 && !intersect(rock, (x - 1, y), &map) {
                        x -= 1;
                    }
                }

                &Jet::R => {
                    // calculate any intersections
                    if x + rock.width < map[0].len() && !intersect(rock, (x + 1, y), &map) {
                        x += 1;
                    }
                }
            }

            if y == rock.height - 1 || intersect(rock, (x, y - 1), &map) {
                break;
            }

            y -= 1;
        }

        top = top.max(y + 1);

        for offset in &rock.shape {
            map[y - offset.1][x + offset.0] = true;
        }

        rockidx += 1;

        if rockidx == 2022 {
            println!("height at 2022 rocks: {}", top);
        }

        if rockidx == target {
            println!("height after 1T rocks: {}", top + multiplied_added);
            break;
        }

        if rockidx > 2022 && multiplied_added == 0 {
            let key: State = (
                jetidx,
                rock.idx,
                map[(top - 20)..top].iter().cloned().collect(),
            );

            match states.entry(key) {
                Entry::Occupied(e) => {
                    let (rock, height) = e.get();

                    let dr = rockidx - rock;
                    let dh = top - height;

                    println!("current rock index: {}", rockidx);

                    println!("found previous section at idx: {} height: {}", rock, height);
                    println!("section of {} rocks of height {} repeats", dr, dh);

                    let remaining = target - rockidx;
                    let mult = remaining / dr;

                    multiplied_added = dh * mult;
                    rockidx += dr * mult;

                    println!("adding {} rocks of {} height", dr * mult, dh * mult);
                }
                Entry::Vacant(e) => {
                    e.insert((rockidx, top));
                }
            };
        }
    }

    println!("height: {}", top);
}

type State<'a> = (usize, usize, Vec<Vec<bool>>);

fn drawmap(map: &Vec<Vec<bool>>) {
    for y in (0..5).rev() {
        for x in 0..7 {
            if map[x][y] {
                print!("#");
            } else {
                print!(".");
            }
        }

        print!("\n");
    }
}
