use std::{
    collections::VecDeque,
    error::Error,
    io::{stdin, BufRead},
    str::FromStr,
};

#[derive(Debug, PartialEq)]
struct Cube {
    x: usize,
    y: usize,
    z: usize,
}

impl FromStr for Cube {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(",").map(|d| d.parse()).flatten();

        Ok(Cube {
            x: iter.next().ok_or("expected x")?,
            y: iter.next().ok_or("expected y")?,
            z: iter.next().ok_or("expected z")?,
        })
    }
}

fn max_coord(cubes: &[Cube]) -> (usize, usize, usize) {
    let mut x = 0;
    let mut y = 0;
    let mut z = 0;

    for cube in cubes {
        x = x.max(cube.x);
        y = y.max(cube.y);
        z = z.max(cube.z);
    }

    (x, y, z)
}

fn face_count(map: &Vec<Vec<Vec<bool>>>, cubes: &[Cube]) -> usize {
    let mut faces = 0;

    let sz = map.len();
    let sy = map[0].len();
    let sx = map[0][0].len();

    for cube in cubes {
        if cube.x == 0 || !map[cube.z][cube.y][cube.x - 1] {
            faces += 1;
        }

        if cube.x == sx - 1 || !map[cube.z][cube.y][cube.x + 1] {
            faces += 1;
        }

        if cube.y == 0 || !map[cube.z][cube.y - 1][cube.x] {
            faces += 1;
        }

        if cube.y == sy - 1 || !map[cube.z][cube.y + 1][cube.x] {
            faces += 1;
        }

        if cube.z == 0 || !map[cube.z - 1][cube.y][cube.x] {
            faces += 1;
        }

        if cube.z == sz - 1 || !map[cube.z + 1][cube.y][cube.x] {
            faces += 1;
        }
    }

    faces
}

fn connected_components(start: &Cube, map: &Vec<Vec<Vec<bool>>>) -> Vec<Vec<Vec<bool>>> {
    let sz = map.len();
    let sy = map[0].len();
    let sx = map[0][0].len();

    let mut component = vec![vec![vec![false; sx]; sy]; sz];
    component[start.z][start.y][start.x] = true;

    let mut queue: VecDeque<(usize, usize, usize)> = VecDeque::new();
    queue.push_back((start.x, start.y, start.z));

    while let Some((x, y, z)) = queue.pop_front() {
        let this = map[z][y][x];

        if x > 0 && !component[z][y][x - 1] && map[z][y][x - 1] == this {
            component[z][y][x - 1] = true;
            queue.push_back((x - 1, y, z));
        }

        if x < sx - 1 && !component[z][y][x + 1] && map[z][y][x + 1] == this {
            component[z][y][x + 1] = true;
            queue.push_back((x + 1, y, z));
        }

        if y > 0 && !component[z][y - 1][x] && map[z][y - 1][x] == this {
            component[z][y - 1][x] = true;
            queue.push_back((x, y - 1, z));
        }

        if y < sy - 1 && !component[z][y + 1][x] && map[z][y + 1][x] == this {
            component[z][y + 1][x] = true;
            queue.push_back((x, y + 1, z));
        }

        if z > 0 && !component[z - 1][y][x] && map[z - 1][y][x] == this {
            component[z - 1][y][x] = true;
            queue.push_back((x, y, z - 1));
        }

        if z < sz - 1 && !component[z + 1][y][x] && map[z + 1][y][x] == this {
            component[z + 1][y][x] = true;
            queue.push_back((x, y, z + 1));
        }
    }

    component
}

fn invert(mut map: Vec<Vec<Vec<bool>>>) -> Vec<Vec<Vec<bool>>> {
    for z in map.iter_mut() {
        for y in z.iter_mut() {
            for x in y.iter_mut() {
                *x = !*x;
            }
        }
    }

    map
}

fn make_cubes(map: &Vec<Vec<Vec<bool>>>) -> Vec<Cube> {
    let cubes: Vec<Cube> = map
        .iter()
        .enumerate()
        .flat_map(|(z, zs)| {
            zs.iter().enumerate().flat_map(move |(y, ys)| {
                ys.iter()
                    .enumerate()
                    .filter(|(_x, px)| **px)
                    .map(move |(x, _px)| Cube { x, y, z })
            })
        })
        .collect();

    cubes
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut cubes: Vec<Cube> = stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap().parse::<Cube>())
        .flatten()
        .collect();

    let (sx, sy, sz) = max_coord(&cubes);

    // surround the entire droplet by 1 layer of air on each side, guaranteeing that the
    // surrounding area is one connected component. hence the + 3 on each axis and + 1 on each
    // coordinate.
    for cube in &mut cubes {
        cube.x += 1;
        cube.y += 1;
        cube.z += 1;
    }

    let mut map = vec![vec![vec![false; sx + 3]; sy + 3]; sz + 3];

    for cube in &cubes {
        map[cube.z][cube.y][cube.x] = true;
    }

    println!("nr of faces: {}", face_count(&map, &cubes));

    let outside = connected_components(&Cube { x: 0, y: 0, z: 0 }, &map);
    let inside = invert(outside);
    let insidecubes = make_cubes(&inside);

    println!("nr of outward faces: {}", face_count(&inside, &insidecubes));

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{make_cubes, Cube};

    #[test]
    fn test_nr_cubes() {
        let mut map = vec![vec![vec![true; 2]; 2]; 2];
        let cubes = make_cubes(&map);

        assert_eq!(cubes.len(), 8);

        map[0][0][0] = false;

        let cubes = make_cubes(&map);

        assert_eq!(cubes.len(), 7);
    }

    #[test]
    fn test_cube_correct() {
        let mut map = vec![vec![vec![true; 2]; 1]; 1];
        let cubes = make_cubes(&map);

        assert_eq!(
            cubes,
            vec![Cube { x: 0, y: 0, z: 0 }, Cube { x: 1, y: 0, z: 0 }]
        );

        map[0][0][1] = false;

        let cubes = make_cubes(&map);

        assert_eq!(cubes, vec![Cube { x: 0, y: 0, z: 0 }]);
    }
}
