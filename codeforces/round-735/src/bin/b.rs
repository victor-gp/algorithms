// https://codeforces.com/contest/1554/problem/B

use std::io::{self, BufReader, prelude::*};

fn main() {
    let stdin_reader = BufReader::new(io::stdin());
    let mut lines = stdin_reader.lines();
    let _ = lines.next();
    while let Some(line) = lines.next() {
        let k = line.unwrap()
            .split_whitespace().skip(1)
            .next().unwrap()
            .parse::<i64>().unwrap();
        let values_str = lines.next().unwrap().unwrap();
        let values = values_str
            .split_whitespace()
            .map(|value_str| value_str.parse::<i64>().unwrap())
            .collect::<Vec<_>>();
        let res = max_cobb(values, k);
        println!("{}", res);
    }
}

fn max_cobb(values: Vec<i64>, k: i64) -> i64 {
    let mut max = cobb((0+1) * (1+1), k, values[0], values[1]);
    for i in 0..values.len() {
        for j in i + 1 ..values.len() {
            let cobb_ij = cobb((i+1)*(j+1), k, values[i], values[j]);
            if cobb_ij > max {
                max = cobb_ij
            }
        }
    }
    max
}

fn cobb(ij: usize, k: i64, ai: i64, aj: i64) -> i64 {
    (ij as i64) - k * (ai | aj)
}
