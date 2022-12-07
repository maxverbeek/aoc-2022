use std::io::{stdin, BufRead};

#[derive(Debug)]
struct FsTree {
    arena: Vec<Node>,
}

#[derive(Debug)]
struct Node {
    children: Vec<usize>,
    parent: Option<usize>,
    size: Option<u32>,
    name: String,
    ftype: FileType,
}

#[derive(Debug, PartialEq)]
enum FileType {
    File,
    Dir,
}

impl Node {
    fn find_child_by_name(&self, name: &str, tree: &FsTree) -> usize {
        for c in self.children.iter() {
            if tree.get_node(*c).name == name {
                return *c;
            }
        }

        unreachable!("correct input must have a child by name")
    }
}

impl FsTree {
    fn new() -> Self {
        FsTree { arena: vec![] }
    }

    fn add_root(&mut self, name: String, size: Option<u32>) -> usize {
        let node = Node {
            children: vec![],
            size,
            name,
            ftype: FileType::Dir,
            parent: None,
        };

        let idx = self.arena.len();

        if idx > 0 {
            return 0;
        }

        self.arena.push(node);
        idx
    }

    fn add_dir(&mut self, parent: usize, name: String) -> usize {
        self.add_node(parent, name, None, FileType::Dir)
    }

    fn add_file(&mut self, parent: usize, name: String, size: u32) -> usize {
        self.add_node(parent, name, Some(size), FileType::File)
    }

    fn add_node(
        &mut self,
        parent: usize,
        name: String,
        size: Option<u32>,
        ftype: FileType,
    ) -> usize {
        let pnode = self.arena.get(parent);

        // check for duplicate dirnames
        for c in pnode.expect("node must exist").children.iter() {
            if self.get_node(*c).name == name {
                return *c;
            }
        }

        let node = Node {
            children: vec![],
            size,
            name,
            parent: Some(parent),
            ftype,
        };

        let idx = self.arena.len();
        self.arena.push(node);

        if let Some(pnode) = self.arena.get_mut(parent) {
            pnode.children.push(idx);
        } else {
            panic!("parent {} does not exist", parent);
        }

        return idx;
    }

    fn get_node(&self, node: usize) -> &Node {
        self.arena
            .get(node)
            .unwrap_or_else(|| panic!("node {} must exist", node))
    }

    fn get_node_mut(&mut self, node: usize) -> &mut Node {
        self.arena
            .get_mut(node)
            .unwrap_or_else(|| panic!("node {} must exist", node))
    }

    fn calcsizes(&mut self, root: usize) -> u32 {
        let rootnode = &self.arena[root];

        let mut sum = 0;

        for c in rootnode.children.clone() {
            let child = &self.arena[c];

            if child.ftype == FileType::Dir {
                let csum = self.calcsizes(c);
                self.arena[c].size = Some(csum);
                sum += csum;
            } else {
                sum += self.arena[c].size.expect("files must have a size");
            }
        }

        return sum;
    }

    fn iter_dirs(&self) -> impl Iterator<Item = &Node> {
        self.arena.iter().filter(|n| n.ftype == FileType::Dir)
    }
}

#[derive(Debug)]
enum InpType {
    Cd(String),
    LsDir(String),
    LsFile(u32, String),
}

fn parse_line(line: &str) -> Option<InpType> {
    if line.starts_with("$ cd") {
        return Some(InpType::Cd(line[5..].into()));
    }

    if line.starts_with("$ ls") {
        return None;
    }

    // at this point, line contains either (size filename) or (dir dirname)
    let mut splitter = line.split(" ");

    let first = splitter.next().unwrap();
    let second = splitter.next().unwrap();

    if first == "dir" {
        return Some(InpType::LsDir(second.to_owned()));
    }

    if let Ok(num) = first.parse::<u32>() {
        return Some(InpType::LsFile(num, second.to_owned()));
    }

    panic!("bad input, reading {}", line)
}

fn main() {
    let mut tree = FsTree::new();

    let mut workdir = 0 as usize;

    for line in stdin()
        .lock()
        .lines()
        .filter_map(|l| parse_line(&l.unwrap()))
    {
        // dbg!(&line);
        match line {
            InpType::Cd(dirname) => {
                if dirname == "/" {
                    workdir = tree.add_root("/".to_owned(), None);
                } else if dirname == ".." {
                    let tnode = tree.get_node(workdir);
                    workdir = tnode.parent.expect("node must have a parent");
                } else {
                    let tnode = tree.get_node(workdir);
                    workdir = tnode.find_child_by_name(&dirname, &tree);
                }
            }
            InpType::LsDir(dirname) => {
                tree.add_dir(workdir, dirname);
            }
            InpType::LsFile(size, filename) => {
                tree.add_file(workdir, filename, size);
            }
        }
    }

    let total_used = tree.calcsizes(0);
    tree.get_node_mut(0).size = Some(total_used);

    let sum = tree
        .iter_dirs()
        .map(|d| {
            d.size
                .unwrap_or_else(|| panic!("dir {} does not have a size yet", d.name))
        })
        .filter(|s| *s < 100000 as u32)
        .sum::<u32>();

    println!("part 1 sum is {}", sum);

    let total_space = 70000000 as u32;
    let required_space = 30000000 as u32;

    let free_space = total_space - total_used;
    let space_to_be_freed = required_space - free_space;

    let mut smallest_dirs = tree
        .iter_dirs()
        .map(|d| d.size.expect("all dirs have a size now"))
        .collect::<Vec<u32>>();

    smallest_dirs.sort();

    let freeme = smallest_dirs
        .into_iter()
        .skip_while(|s| s < &space_to_be_freed)
        .nth(0);

    println!("part 2 size is {}", freeme.expect("you must exist"));
}
