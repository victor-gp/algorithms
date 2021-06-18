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

        let action = next_action(&global, &mut me, &opp);

        println!("{}", action);
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
    pos: Coord,
    visited: Vec<Coord>,
    torpedo_cooldown: i32,
    // sonar_cooldown: i32,
    // silence_cooldown: i32,
    // mine_cooldown: i32,
}

struct Opponent {
    lives: i32,
    // likely_pos: Iter<'static, Coord>,
    // visited: Vec<Coord>,
    // move_history: Vec<Move>,
    // cooldowns
}

struct Map {
    width: i32,
    height: i32,
    grid: Vec<Vec<Cell>>,
    water: Vec<Coord>,
}

#[derive(Copy, Clone)]
struct Coord {
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

    me.pos = Coord{x, y};
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

impl fmt::Display for Coord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.x, self.y)
    }
}

impl fmt::Debug for Coord {
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

    fn is_water(&self) -> bool {
        matches!(self, Cell::Water)
    }
}

use std::slice::Iter;

impl Map {
    fn new(width: i32, height: i32, grid: Vec<Vec<Cell>>) -> Map {
        let water = Map::water_positions(&grid);

        Map { width, height, grid, water }
    }

    fn water_positions(grid: &Vec<Vec<Cell>>) -> Vec<Coord> {
        grid.iter().enumerate().flat_map(|ir| {
            let (i, row) = ir;
            row.iter().enumerate().filter_map(move |jc| {
                let (j, cell) = jc;
                match cell {
                    Cell::Water => Some( Coord{x: j, y: i} ),
                    Cell::Land  => None
                }
            })
        }).collect::<Vec<Coord>>()
    }

    fn water_it(&self) -> Iter<Coord> {
        self.water.iter()
    }
}

impl Me {
    fn new() -> Me {
        Me {
            lives: 0,
            pos: Coord{x: 0, y: 0},
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

#[derive(Copy, Clone)]
enum Action {
    Move { dir: char },
    Surface,
    Torpedo { pos: Coord },
    // Msg { message: &'static str },
}

impl Action {
    fn possible_moves(pos: Coord, map: &Map, visited: &Vec<Coord>) -> Vec<Action> {
        let directions = ['N', 'E', 'S', 'W'];
        let adjacents = pos.adjacents_clockwise();
        let mut valid_moves = Vec::new();

        eprintln!("{:?}", adjacents);

        for i in 0..=3 {
            let pos_i = adjacents[i];
            if map.is_water(pos_i) && !visited.contains(&pos_i) {
                valid_moves.push(
                    Action::Move{ dir: directions[i] }
                );
            }
        }

        eprintln!("{:?}", valid_moves);

        valid_moves
    }
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Action::Move{dir} => write!(f, "MOVE {} TORPEDO", dir),
            Action::Surface => write!(f, "SURFACE"),
            Action::Torpedo{pos} => write!(f, "TORPEDO {}", pos),
        }
    }
}

impl fmt::Debug for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Coord {
    fn adjacents_clockwise(&self) -> [Coord; 4] {
        [
            Coord { y: self.y - 1, ..*self },
            Coord { x: self.x + 1, ..*self },
            Coord { y: self.y + 1, ..*self },
            Coord { x: self.x - 1, ..*self }
        ]
    }
}

impl PartialEq for Coord {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}

impl Map {
    fn is_water(&self, coord: Coord) -> bool {
        match self.cell_at(coord) {
            None => false,
            Some(cell) => cell.is_water()
        }
    }

    fn is_within_bounds(&self, coord: Coord) -> bool {
        0 <= coord.x && coord.x < self.width as usize
            && 0 <= coord.y && coord.y < self.height as usize
    }

    fn cell_at(&self, coord: Coord) -> Option<&Cell> {
        if self.is_within_bounds(coord) {
            Some( &self.grid[coord.y][coord.x] )
        } else {
            None
        }
    }
}

fn next_action(global: &Global, me: &mut Me, opp: &Opponent) -> Action {
    let possible_moves = Action::possible_moves(me.pos, &global.map, &me.visited);

    if possible_moves.is_empty() {
        me.visited = Vec::new();
        Action::Surface
    } else {
        possible_moves[0]
    }
}
