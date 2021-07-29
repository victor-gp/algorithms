// https://codeforces.com/contest/1554/problem/A

use std::io::{self, BufReader, prelude::*};

// trick: we only need to count pairs, no need to consider larger subsets

fn main() {
    let stdin_reader = BufReader::new(io::stdin());
    let mut lines = stdin_reader.lines();
    let _ = lines.next();
    while let Some(_) = lines.next() {
        let values_str = lines.next().unwrap().unwrap();
        let values = values_str
            .split_whitespace()
            .map(|value_str| value_str.parse::<i64>().unwrap())
            .collect::<Vec<_>>();
        let res = max_pair_prod(values);
        println!("{}", res);
    }
}

fn max_pair_prod(values: Vec<i64>) -> i64 {
    let pairs_iter = values.iter().zip(values.iter().skip(1));
    let prods_iter = pairs_iter.map(|(a, b)| a*b);
    prods_iter.max().unwrap()
}
