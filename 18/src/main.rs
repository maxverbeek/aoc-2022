use std::{
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

type Vec3<T> = Vec<Vec<Vec<T>>>;

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

fn face_count(map: &Vec3<bool>, cubes: &[Cube]) -> usize {
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

fn find_labels(map: &Vec3<bool>) -> Vec3<usize> {
    let sz = map.len();
    let sy = map[0].len();
    let sx = map[0][0].len();

    let mut labels = vec![vec![vec![0; sx]; sy]; sz];
    let mut nextlabel = 0;
    let mut equivalences = Vec::new();

    for z in 0..sz {
        for y in 0..sy {
            for x in 0..sx {
                let mut neighbours: [usize; 3] = [0; 3];
                let mut nlen = 0;
                let thispx = map[z][y][x];

                if x > 0 && map[z][y][x - 1] == thispx {
                    neighbours[nlen] = labels[z][y][x - 1];
                    nlen += 1;
                }

                if y > 0 && map[z][y - 1][x] == thispx {
                    neighbours[nlen] = labels[z][y - 1][x];
                    nlen += 1;
                }

                if z > 0 && map[z - 1][y][x] == thispx {
                    neighbours[nlen] = labels[z - 1][y][x];
                    nlen += 1;
                }

                if nlen == 0 {
                    // there are no neighbouring labels yet, create a new one and assign it to this
                    // pixel
                    labels[z][y][x] = nextlabel;

                    equivalences.push(nextlabel);
                    nextlabel += 1;
                } else {
                    // use the smallest label that we find here as the "root" label, and union all
                    // other neighbour sets with this label.
                    let smallest = *neighbours[0..nlen].iter().min().expect("nlen > 0");
                    labels[z][y][x] = smallest;

                    for &l in &neighbours[0..nlen] {
                        union_sets(&mut equivalences, smallest, l);
                    }
                }
            }
        }
    }

    for z in 0..sz {
        for y in 0..sy {
            for x in 0..sx {
                labels[z][y][x] = find_root(labels[z][y][x], &mut equivalences);
            }
        }
    }

    labels
}

fn find_root(label: usize, equivalences: &mut [usize]) -> usize {
    let mut root = label;

    while equivalences[root] != root {
        (root, equivalences[root]) = (equivalences[root], equivalences[equivalences[root]]);
    }

    root
}

fn union_sets(equivalences: &mut [usize], x: usize, y: usize) {
    let rx = find_root(x, equivalences);
    let ry = find_root(y, equivalences);

    if rx == ry {
        return;
    }

    let root = rx.max(ry);

    equivalences[rx] = root;
    equivalences[ry] = root;
}

fn make_cubes(map: &Vec3<bool>) -> Vec<Cube> {
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

fn make_inside_map(labels: &Vec3<usize>) -> Vec3<bool> {
    let sz = labels.len();
    let sy = labels[0].len();
    let sx = labels[0][0].len();

    let outterlabel = labels[0][0][0];

    let mut inside = vec![vec![vec![false; sx]; sy]; sz];

    for z in 0..(sz) {
        for y in 0..(sy) {
            for x in 0..(sx) {
                inside[z][y][x] = labels[z][y][x] != outterlabel;
            }
        }
    }

    inside
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

    let labels = find_labels(&map);
    let inside = make_inside_map(&labels);
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
