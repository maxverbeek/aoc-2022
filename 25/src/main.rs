use std::io::{stdin, BufRead};

fn snafu_to_decimal(snafu: &str) -> isize {
    let mut radix = 1;
    let mut res = 0;

    for c in snafu.chars().rev() {
        let val = match c {
            '0'..='2' => c.to_digit(10).unwrap() as isize,
            '-' => -1,
            '=' => -2,
            _ => panic!("{} contains non digits ({})", snafu, c),
        };

        res += val * radix;
        radix *= 5;
    }

    res
}

fn decimal_to_snafu(mut dec: isize) -> String {
    let mut res = String::new();

    while dec != 0 {
        let snafudigit = (dec + 2) % 5 - 2;

        let snchar = match snafudigit {
            -2 => '=',
            -1 => '-',
            0..=2 => (snafudigit as u8 + '0' as u8) as char,
            _ => unreachable!("digit cannot be {}", snafudigit),
        };

        res = format!("{}{}", snchar, res);
        dec = (dec - snafudigit) / 5;
    }

    res
}

fn main() {
    let lines = stdin().lock().lines().map(|l| l.unwrap());

    let sum = lines.map(|l| snafu_to_decimal(&l)).sum::<isize>();

    println!("sum: {}", decimal_to_snafu(sum));
}

#[cfg(test)]
mod tests {
    use crate::{decimal_to_snafu, snafu_to_decimal};

    #[test]
    fn first_example() {
        let res = snafu_to_decimal("1121-1110-1=0");
        assert_eq!(res, 314159265);
    }

    #[test]
    fn whatisthis() {
        let res = snafu_to_decimal("1==");
        assert_eq!(res, 13);

        assert_eq!(snafu_to_decimal("2"), 2);
        assert_eq!(snafu_to_decimal("1="), 3);
        assert_eq!(snafu_to_decimal("22"), 12);
        assert_eq!(snafu_to_decimal("222"), 62);
        assert_eq!(snafu_to_decimal("2222"), 312);
        assert_eq!(snafu_to_decimal("1===="), 313);
    }

    #[test]
    fn dec_to_snafu() {
        assert_eq!(decimal_to_snafu(2), "2");
        assert_eq!(decimal_to_snafu(3), "1=");
        assert_eq!(decimal_to_snafu(12), "22");
        assert_eq!(decimal_to_snafu(13), "1==");
        assert_eq!(decimal_to_snafu(314159265), "1121-1110-1=0");
    }
}
