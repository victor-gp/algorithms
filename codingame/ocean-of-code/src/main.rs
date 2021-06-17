use std::io;
use rand::Rng;

macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

fn main() {
    let mut global = read_starting_info();

    let mut rng = rand::thread_rng();

    // 1st turn: choose a starting position
    let valid_ps = global.map.water_positions();
    let first_pos = valid_ps[
        rng.gen_range(0..valid_ps.len())
    ];
    println!("{}", display(first_pos));

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

struct Map {
    width: i32,
    height: i32,
    grid: Vec<Vec<Cell>>,
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
    likely_pos: Vec<Position>,
    //visited: Vec<Position>,
    //move_history: Vec<Move>,
    // cooldowns
}

fn read_starting_info() -> Global {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();
    let width = parse_input!(inputs[0], i32);
    let height = parse_input!(inputs[1], i32);
    let my_id = parse_input!(inputs[2], i32);

    let map = Map::read(width, height);
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

    if global.turn > 2 {
        me.visited.push(me.pos)

        // TODO: analyze everything
    }

    me.pos = (y as usize, x as usize);
    me.lives = my_life;
    opp.lives = opp_life;

    me.torpedo_cooldown = torpedo_cooldown;
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

impl Map {
    fn water_positions(&self) -> Vec<Position> {
        self.grid.iter().enumerate().flat_map(|ir| {
            let (i, row) = ir;
            row.iter().enumerate().filter_map(move |jc| {
                let (j, cell) = jc;
                match cell {
                    Cell::Water => Some((j,i)), // transpose!
                    Cell::Land  => None
                }
            })
        }).collect::<Vec<Position>>()
    }
}

type Position = (usize, usize);

fn display(p: Position) -> String {
    let (x, y) = p;
    format!("{} {}", x, y)
}

enum Cell { Water, Land }

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

impl Me {
    fn new() -> Me {
        Me {
            lives: 0,
            pos: (0, 0),
            visited: Vec::new(),
            torpedo_cooldown: 0,
        }
    }
}

impl Opponent {
    fn new() -> Opponent {
        Opponent {
            lives: 0,
            likely_pos: Vec::new(),
        }
    }
}
