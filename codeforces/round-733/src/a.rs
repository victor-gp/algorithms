use std::io;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
	let mut input = String::new();
    io::stdin().read_line(&mut input)?;
	let cases = input.trim().parse::<u32>()?;
    for _ in 0..cases {
        input.clear();
        io::stdin().read_line(&mut input)?;
        let mut n = input.trim().parse::<u32>()?;
        let mut max_digit = 0;
        while n != 0 {
            let d = n%10;
            if d > max_digit {
                max_digit = d;
            }
            n /= 10;
        }
        println!("{}", max_digit)
    }

    Ok(())
}


fn input() -> i32 {
	let mut input = String::new();
	io::stdin().read_line(&mut input).unwrap();
	let value = input.trim().parse::<i32>().unwrap();
	value
}

fn inputvec() -> Vec<i32> {
	let mut input = String::new();
	input = input.trim().to_string();
	io::stdin().read_line(&mut input).unwrap();
	let values: Vec<i32> = input
		.split_whitespace()
		.map(|s| s.parse().unwrap())
		.collect();
	values
}
