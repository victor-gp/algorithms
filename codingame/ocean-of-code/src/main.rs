use std::io;
macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

use rand::seq::IteratorRandom;

fn main() {
    let mut global = read_starting_info();

    // 1st turn: choose a starting position
    let first_pos = global.map.water_it().choose(&mut rand::thread_rng()).unwrap();
    println!("{}", first_pos);

    let mut me = Me::new();
    let mut opp = Opponent::new();

    loop {
        global.turn += 1;
        read_turn_info(&global, &mut me, &mut opp);

        println!("MOVE N TORPEDO");
    }
}

struct Global {
    map: Map,
    turn: i32,
    game_turns: i32,
    me_first: bool,
}

struct Me {
    lives: i32,
    pos: Position,
    visited: Vec<Position>,
    torpedo_cooldown: i32,
    // sonar_cooldown: i32,
    // silence_cooldown: i32,
    // mine_cooldown: i32,
}

struct Opponent {
    lives: i32,
    // likely_pos: Iter<'static, Position>,
    // visited: Vec<Position>,
    // move_history: Vec<Move>,
    // cooldowns
}

struct Map {
    width: i32,
    height: i32,
    grid: Vec<Vec<Cell>>,
    water: Vec<Position>,
}

#[derive(Copy, Clone)]
struct Position {
    x: usize,
    y: usize
}

enum Cell { Water, Land }

fn read_starting_info() -> Global {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();
    let width = parse_input!(inputs[0], i32);
    let height = parse_input!(inputs[1], i32);
    let my_id = parse_input!(inputs[2], i32);

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

    let map = Map::new(width, height, grid);
    eprintln!("{:?}", map);

    Global {
        map,
        turn: 1,
        game_turns: 300,
        me_first: my_id == 0
    }
}

fn read_turn_info(global: &Global, me: &mut Me, opp: &mut Opponent) {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();
    let x = parse_input!(inputs[0], usize);
    let y = parse_input!(inputs[1], usize);
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

    if global.turn > 2 {
        me.visited.push(me.pos)

        // TODO: analyze everything
    }

    me.pos = Position{x, y};
    me.lives = my_life;
    opp.lives = opp_life;

    me.torpedo_cooldown = torpedo_cooldown;
}

use std::fmt;

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let preamble = format!("Map {{\nwidth: {} / height: {}\n", self.width, self.height);
        let grid = self.grid.iter()
            .map(|row| row.iter()
                .map(|cell| cell.to_char())
                .collect::<String>()
            ).collect::<Vec<String>>()
            .join("\n");
        let slice = &(preamble + &grid + "\n} Map");

        f.write_str(slice)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.x, self.y)
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", (self.x, self.y))
    }
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

use std::slice::Iter;

impl Map {
    fn new(width: i32, height: i32, grid: Vec<Vec<Cell>>) -> Map {
        let water = Map::water_positions(&grid);

        Map { width, height, grid, water }
    }

    fn water_positions(grid: &Vec<Vec<Cell>>) -> Vec<Position> {
        grid.iter().enumerate().flat_map(|ir| {
            let (i, row) = ir;
            row.iter().enumerate().filter_map(move |jc| {
                let (j, cell) = jc;
                match cell {
                    Cell::Water => Some( Position{x: j, y: i} ),
                    Cell::Land  => None
                }
            })
        }).collect::<Vec<Position>>()
    }

    fn water_it(&self) -> Iter<Position> {
        self.water.iter()
    }
}

impl Me {
    fn new() -> Me {
        Me {
            lives: 0,
            pos: Position{x: 0, y: 0},
            visited: Vec::new(),
            torpedo_cooldown: 0,
        }
    }
}

impl Opponent {
    fn new() -> Opponent {
        Opponent {
            lives: 0,
        }
    }
}
