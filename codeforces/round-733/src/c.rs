use std::io;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
	let cases = input_token().parse::<u32>()?;
    for _ in 0..cases {
        let stages = input_token().parse::<u32>().unwrap();
        let mut my_scores: Vec<u32> = input_tokens().iter().map(|s| s.parse::<u32>().unwrap()).collect();
        let mut their_scores: Vec<u32> = input_tokens().iter().map(|s| s.parse::<u32>().unwrap()).collect();
        my_scores.sort();
        their_scores.sort();
        let mut i = stages as usize / 4;
        let mut my_sum: u32 = my_scores[i..].iter().sum();
        let mut their_sum: u32 = their_scores[i..].iter().sum();
        let mut extra_stages = 0;
        let mut j = i;

        while my_sum < their_sum && j > 0 {
            extra_stages += 1;
            my_sum += 100 - my_scores[i];
            i += 1;
            their_sum += their_scores[j];
            j -= 1;

            if (stages + extra_stages) % 4 == 0 {
                my_sum -= my_scores[i];
                i += 1;
                their_sum -= their_scores[j];
                j += 1;
            }
        }

        while my_sum < their_sum && i < stages as usize {
            extra_stages += 1;
            my_sum += 100 - my_scores[i];
            i += 1;

            if (stages + extra_stages) % 4 == 0 && i < stages as usize {
                my_sum -= my_scores[i];
                i += 1;
            }
        }

        if my_sum < their_sum {
            extra_stages += (their_sum - my_sum) / 100
        }

        println!("{}", extra_stages);
    }

    Ok(())
}

fn input_token() -> String {
	let mut input = String::new();
	io::stdin().read_line(&mut input).unwrap();
	let token = input.trim().to_string();
	token
}

fn input_tokens() -> Vec<String> {
	let mut input = String::new();
	io::stdin().read_line(&mut input).unwrap();
	let tokens = input.split_whitespace().map(str::to_string).collect();
    tokens
}
