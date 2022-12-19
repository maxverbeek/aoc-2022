use std::collections::HashMap;

use crate::types::Blueprint;

type Key = (usize, usize, usize, usize, usize, usize, usize);

pub fn find_geodes(
    bp: &Blueprint,
    memo: &mut HashMap<Key, usize>,
    time: usize,
    ore: usize,
    clay: usize,
    obsidian: usize,
    orebots: usize,
    claybots: usize,
    obsidianbots: usize,
) -> usize {
    if time == 0 {
        return 0;
    }

    // println!("t={time}, ore: {ore}, clay: {clay}, obsidian: {obsidian}, orebots: {orebots}, claybots: {claybots}, obsbots: {obsidianbots}, geobots: {geobots}");

    let key: Key = (time, ore, clay, obsidian, orebots, claybots, obsidianbots);

    if let Some(&ans) = memo.get(&key) {
        return ans;
    }

    let mut optimal = 0;

    let ore = ore + orebots;
    let clay = clay + claybots;
    let obsidian = obsidian + obsidianbots;

    // maybe build an orebot
    if ore - orebots >= bp.orebot {
        let ore = ore - bp.orebot;
        let orebots = orebots + 1;

        let ans = find_geodes(
            bp,
            memo,
            time - 1,
            ore,
            clay,
            obsidian,
            orebots,
            claybots,
            obsidianbots,
        );

        optimal = optimal.max(ans);
    }

    // maybe build a clay bot
    if ore - orebots >= bp.claybot {
        let ore = ore - bp.claybot;
        let claybots = claybots + 1;

        let ans = find_geodes(
            bp,
            memo,
            time - 1,
            ore,
            clay,
            obsidian,
            orebots,
            claybots,
            obsidianbots,
        );

        optimal = optimal.max(ans);
    }

    // maybe build an obsidian bot
    if ore - orebots >= bp.obsidianbot.0 && clay - claybots >= bp.obsidianbot.1 {
        let ore = ore - bp.obsidianbot.0;
        let clay = clay - bp.obsidianbot.1;
        let obsidianbots = obsidianbots + 1;

        let ans = find_geodes(
            bp,
            memo,
            time - 1,
            ore,
            clay,
            obsidian,
            orebots,
            claybots,
            obsidianbots,
        );
        optimal = optimal.max(ans);
    }

    // maybe build geode bot
    if ore - orebots >= bp.geodebot.0 && obsidian - obsidianbots >= bp.geodebot.1 {
        let ore = ore - bp.geodebot.0;
        let obsidian = obsidian - bp.geodebot.1;

        let ans = time
            + find_geodes(
                bp,
                memo,
                time - 1,
                ore,
                clay,
                obsidian,
                orebots,
                claybots,
                obsidianbots,
            );

        optimal = optimal.max(ans);
    }

    // build no bots
    {
        let ans = find_geodes(
            bp,
            memo,
            time - 1,
            ore,
            clay,
            obsidian,
            orebots,
            claybots,
            obsidianbots,
        );

        optimal = optimal.max(ans);
    }

    memo.insert(key, optimal);
    optimal
}
