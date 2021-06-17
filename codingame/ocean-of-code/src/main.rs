use std::io;

macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

const GAME_TURNS: u16 = 300;

fn main() {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();
    let width = parse_input!(inputs[0], i32);
    let height = parse_input!(inputs[1], i32);
    let my_id = parse_input!(inputs[2], i32);
    let map = Map::read(width, height);
    // eprintln!("{}", map.to_string());

    let mut turn = 1u16;

    // starting position, turn 1
    println!("7 7");

    // game loop
    loop {
        turn += 1;

        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let inputs = input_line.split(" ").collect::<Vec<_>>();
        let x = parse_input!(inputs[0], i32);
        let y = parse_input!(inputs[1], i32);
        let my_life = parse_input!(inputs[2], i32);
        let opp_life = parse_input!(inputs[3], i32);
        let torpedo_cooldown = parse_input!(inputs[4], i32);
        let sonar_cooldown = parse_input!(inputs[5], i32);
        let silence_cooldown = parse_input!(inputs[6], i32);
        let mine_cooldown = parse_input!(inputs[7], i32);
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let sonar_result = input_line.trim().to_string();
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let opponent_orders = input_line.trim_matches('\n').to_string();

        println!("MOVE N TORPEDO");
    }
}

struct Map {
    width: i32,
    height: i32,
    grid: Vec<Vec<Cell>>,
}

impl Map {
    fn read(width: i32, height: i32) -> Map {
        let mut grid = Vec::new();

        for i in 0..height as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let line = input_line.trim_matches('\n').to_string();
            // eprintln!("{}",line);

            grid.push(
                line.chars()
                .map(|c| Cell::from_char(c))
                .collect::<Vec<Cell>>()
            )
        }

        Map { width, height, grid }
    }

    fn to_string(&self) -> String {
        let preamble = format!("<Map>\nwidth: {} / height: {}\n", self.width, self.height);
        let grid = self.grid.iter()
            .map(|row| row.iter()
                .map(|cell| cell.to_char())
                .collect::<String>()
            ).collect::<Vec<String>>()
            .join("\n");

        preamble + &grid + "\n</Map>"
    }
}

enum Cell {
    Water, Land
}

impl Cell {
    fn from_char(c: char) -> Cell {
        match c {
            '.' => Cell::Water,
            'x' => Cell::Land,
            _ => panic!("invalid cell")
        }
    }

    fn to_char(&self) -> char {
        match self {
            Cell::Water => '.',
            Cell::Land  => 'x'
        }
    }
}
