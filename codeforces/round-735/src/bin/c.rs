// https://codeforces.com/contest/1554/problem/B

use std::io::{self, BufReader, prelude::*};

fn main() {
    let stdin_reader = BufReader::new(io::stdin());
    let mut lines = stdin_reader.lines();
    let _ = lines.next();
    while let Some(line) = lines.next() {
        let nm = line.unwrap()
            .split_whitespace();
        let n = nm.next().unwrap();
        let m = nm.next().unwrap();
        let mikasa = mikasa(n, m);
        println!(mikasa);
    }
}

fn mikasa(mut n: i64, m: i64) {
    let mut min = 0;
    let mut ans = 0;
    while n != 0 {
        if largest_bit(n) < m {
            min = largest_bit(n)
        }
        else {
            ans = 2 * ans + 1;
        }
    }
}

fn largest_bit(mut n: i64) {
    if n == 0 {
        return 0;
    }
    let mut ret = 1;
    n /= 2;
    while n != 0 {
        n /= 2;
        ret *= 2;
    }
    ret
}
