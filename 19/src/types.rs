use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct Blueprint {
    pub idx: usize,
    pub orebot: usize,
    pub claybot: usize,
    pub obsidianbot: (usize, usize),
    pub geodebot: (usize, usize),
}

impl FromStr for Blueprint {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut nums = s
            .split(['\n', ':', ' '])
            .flat_map(|word| word.parse::<usize>());

        Ok(Blueprint {
            idx: nums.next().ok_or("expected idx")?,
            orebot: nums.next().ok_or("expected ore cost")?,
            claybot: nums.next().ok_or("expected clay cost")?,
            obsidianbot: (
                nums.next().ok_or("expected obsidian ore cost")?,
                nums.next().ok_or("expected obsidian clay cost")?,
            ),
            geodebot: (
                nums.next().ok_or("expected geobot ore cost")?,
                nums.next().ok_or("expected geobot obs cost")?,
            ),
        })
    }
}
