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
