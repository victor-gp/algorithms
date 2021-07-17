use std::io;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
	let mut input = String::new();
    io::stdin().read_line(&mut input)?;
	let cases = input.trim().parse::<u32>()?;
    for _ in 0..cases {
        input.clear();
        io::stdin().read_line(&mut input)?;
        let mut h_w = input.split_whitespace().map(|s| s.parse::<u32>());
        let height = h_w.next().unwrap()?;
        let width = h_w.next().unwrap()?;

        print_side_row(width);
        for i in 1..height-1 {
            if i % 2 == 0 && i < height - 2 {
                print_plated_row(width);
            } else {
                print_empty_row(width);
            }
        }
        print_side_row(width);

        println!()
    }

    Ok(())
}

fn print_side_row(width: u32) {
    for j in 0..width {
        if j % 2 == 0 {
            print!("1");
        } else {
            print!("0");
        }
    }
    println!()
}

fn print_empty_row(width: u32) {
    print_inside(width);
    println!();
}

fn print_plated_row(width: u32) {
    print!("1");
    print_inside(width - 2);
    println!("1");
}

fn print_inside(width: u32) {
    for _ in 0..width {
        print!("0");
    }
}


// fn input_token() -> String {
// 	let mut input = String::new();
// 	io::stdin().read_line(&mut input).unwrap();
// 	let value = input.trim().parse::<i32>().unwrap();
// 	value
// }

// fn input_tokens() -> Vec<String> {
// 	let mut input = String::new();
// 	input = input.trim().to_string();
// 	io::stdin().read_line(&mut input).unwrap();
// 	let values: Vec<i32> = input
// 		.split_whitespace()
// 		.map(|s| s.parse().unwrap())
// 		.collect();
// 	values
// }
