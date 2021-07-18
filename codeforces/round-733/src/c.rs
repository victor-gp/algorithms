use std::io;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
	let cases = input_token().parse::<u32>()?;
    for _ in 0..cases {
        let stages = input_token().parse::<usize>().unwrap();
        let mut my_scores: Vec<u32> = input_tokens().iter().map(|s| s.parse::<u32>().unwrap()).collect();
        let mut their_scores: Vec<u32> = input_tokens().iter().map(|s| s.parse::<u32>().unwrap()).collect();
        my_scores.sort();
        their_scores.sort();
        let mut i = stages / 4;
        let mut my_sum: u32 = my_scores[i..].iter().sum();
        let mut their_sum: u32 = their_scores[i..].iter().sum();
        let mut extra_stages = 0;
        let mut j = i;

        while my_sum < their_sum && j > 0 {
            extra_stages += 1;
            my_sum += 100;
            if (stages + extra_stages) % 4 == 0 {
                my_sum -= my_scores[i];
                i += 1;
                // j doesn't change, the score that would be added is dropped
                // cause now it belongs in the (total_stages / 4).floor lowest scores
            } else {
                // j is init-ed to i, which points to an already added score
                // cannot init j = i-1 cause it panics when i = 0 (j is usize)
                j -= 1;
                their_sum += their_scores[j];
            }
        }

        // no need to safecheck i, the upper bound on extra_stages is stages.
        // by then, i would be stages/2, which is halfway down my_scores[]
        while my_sum < their_sum {
            extra_stages += 1;
            my_sum += 100;
            if (stages + extra_stages) % 4 == 0 {
                my_sum -= my_scores[i];
                i += 1;
            }
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
