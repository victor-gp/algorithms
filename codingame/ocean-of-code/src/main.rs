use std::io;

macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

use rand::seq::IteratorRandom;

fn main() {
    let mut timer = Timer::new();

    let mut global = read_starting_info();
    let mut me = Me::new();
    let mut opp = Opponent::new();

    // 1st turn: choose a starting position
    let first_pos = global.map.water_it().choose(&mut rand::thread_rng()).unwrap();
    println!("{}", first_pos);

    timer.setup_stop();

    loop {
        timer.start();

        global.turn += 1;
        read_turn_info(&global, &mut me, &mut opp);

        let action = next_action(&global, &mut me, &opp);
        println!("{}", action);

        timer.stop(global.turn - 1);
    }
}

fn next_action(global: &Global, me: &mut Me, opp: &Opponent) -> Action {
    let viable_moves = Action::viable_moves(me.pos, &global.map, &me.visited);

    if viable_moves.is_empty() {
        me.visited = Vec::new();
        Action::Surface
    } else {
        viable_moves[0]
    }
}

#[allow(dead_code)]
struct Global {
    map: Map,
    turn: usize,
    game_turns: usize,
    me_first: bool,
}

struct Me {
    lives: usize,
    pos: Coord,
    visited: Vec<Coord>,
    torpedo_cooldown: i32,
    // sonar_cooldown: i32,
    // silence_cooldown: i32,
    // mine_cooldown: i32,
}

struct Opponent {
    lives: usize,
    feasible_ps: Vec<Coord>,
    // visited: Vec<Coord>,
    // move_history: Vec<Move>,
    // cooldowns
}

#[derive(Copy, Clone)]
enum Action {
    Move { dir: char },
    Surface,
    Torpedo { target: Coord },
    // Msg { message: &'static str },
}

struct Map {
    width: usize,
    height: usize,
    grid: Vec<Vec<Cell>>,
    water: Vec<Coord>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct Coord {
    x: usize,
    y: usize
}

enum Cell { Water, Land }

use std::time::{Duration, Instant};

struct Timer {
    start: Instant,
    turns_acc: Duration
}

fn read_starting_info() -> Global {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();
    let width = parse_input!(inputs[0], usize);
    let height = parse_input!(inputs[1], usize);
    let my_id = parse_input!(inputs[2], usize);

    let mut grid = Vec::new();
    for _ in 0..height {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let line = input_line.trim_matches('\n').to_string();
        // eprintln!("{}",line);

        let cells_row = line.chars()
            .map(|c| Cell::from_char(c))
            .collect::<Vec<Cell>>();
        grid.push(cells_row);
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

#[allow(unused_variables)]
fn read_turn_info(global: &Global, me: &mut Me, opp: &mut Opponent) {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();
    let x = parse_input!(inputs[0], usize);
    let y = parse_input!(inputs[1], usize);
    let my_life = parse_input!(inputs[2], usize);
    let opp_life = parse_input!(inputs[3], usize);
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
        me.visited.push(me.pos);

        let opp_actions = OppAction::seq_from_str(&opponent_orders);
        opp.analyze_actions(&global.map, &opp_actions);

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

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Action::Move{dir} => write!(f, "MOVE {} TORPEDO", dir),
            Action::Surface => write!(f, "SURFACE"),
            Action::Torpedo{target} => write!(f, "TORPEDO {}", target),
        }
    }
}

impl fmt::Debug for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Timer {
    fn new() -> Timer {
        Timer {
            start: Instant::now(),
            turns_acc: Duration::new(0,0)
        }
    }

    fn start(&mut self) {
        self.start = Instant::now();
    }

    fn setup_stop(&self) {
        eprintln!("Response time: {} ms", self.start.elapsed().as_millis())
    }

    fn stop(&mut self, turn: usize) {
        let elapsed = self.start.elapsed();
        self.turns_acc += elapsed;
        let mean = self.turns_acc.div_f32(turn as f32);

        eprintln!("Response time: {} ms / Mean: {} ms",
            elapsed.as_millis(), mean.as_millis());
    }
}

use std::slice::Iter;

impl Map {
    fn new(width: usize, height: usize, grid: Vec<Vec<Cell>>) -> Map {
        let water = Map::water_positions(&grid);

        Map { width, height, grid, water }
    }

    fn cell_at(&self, coord: Coord) -> Option<&Cell> {
        if self.is_within_bounds(coord) {
            Some( &self.grid[coord.y][coord.x] )
        } else {
            None
        }
    }

    fn is_within_bounds(&self, coord: Coord) -> bool {
        coord.x < self.width
            && coord.y < self.height
    }

    fn is_water(&self, coord: Coord) -> bool {
        match self.cell_at(coord) {
            None => false,
            Some(cell) => cell.is_water()
        }
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
            feasible_ps: Vec::new(),
        }
    }
}

impl Action {
    fn viable_moves(pos: Coord, map: &Map, visited: &Vec<Coord>) -> Vec<Action> {
        let directions = ['N', 'E', 'S', 'W'];
        let adjacents = pos.adjacents_clockwise();
        let mut viable_moves = Vec::new();

        for i in 0..=3 {
            let pos_i = adjacents[i];
            if map.is_water(pos_i) && !visited.contains(&pos_i) {
                viable_moves.push(
                    Action::Move{ dir: directions[i] }
                );
            }
        }

        viable_moves
    }
}

enum OppAction {
    Move { dir: char },
    Surface { sector: usize },
    Torpedo { target: Coord },
}

impl OppAction {
    fn from_str(s: &str) -> Option<Self> {
        let mut tokens = s.split_whitespace();
        let token = tokens.next()?;
        match token {
            "MOVE" => {
                let dir = parse_input!(tokens.next()?, char);
                Some( Self::Move{ dir } )
            },
            "SURFACE" => {
                let sector = parse_input!(tokens.next()?, usize);
                Some( Self::Surface{ sector: sector } )
            },
            "TORPEDO" => {
                let x = parse_input!(tokens.next()?, usize);
                let y = parse_input!(tokens.next()?, usize);
                Some( Self::Torpedo{ target: Coord {x,y} } )
            },
            // TODO "MSG" => None // no need for this? it's not on the opponent's API?
            _ => {
                eprintln!("Action::from_str: could not parse string \"{}\"", s);
                None
            }
        }
    }

    fn seq_from_str(action_seq: &str) -> Vec<Self> {
        action_seq.split("|").filter_map(|s| Self::from_str(s)).collect()
        // return iter?
    }

    #[allow(dead_code)]
    fn string_from_seq(seq: Vec<Self>) -> String {
        seq.iter().map(|action| {
            format!("{:?}", action)
        }).collect::< Vec<String> >().join("|")
    }
}

impl fmt::Debug for OppAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Move{dir} => write!(f, "MOVE {}", dir),
            Self::Surface{sector} => write!(f, "SURFACE {}", sector),
            Self::Torpedo{target} => write!(f, "TORPEDO {}", target),
        }
    }
}

impl Opponent {
    fn analyze_actions(&mut self, map: &Map, actions: &Vec<OppAction>) {
        for action in actions {
            match action {
                OppAction::Move{dir} => {
                    if !self.feasible_ps.is_empty() {
                        self.feasible_ps.retain(
                            |pos| map.is_viable_move(*pos, *dir)
                        );

                        for i in 0 .. self.feasible_ps.len() {
                            self.feasible_ps[i] = self.feasible_ps[i]._move(*dir)
                        }
                    }
                },
                OppAction::Surface{sector} => {
                    if self.feasible_ps.is_empty() {
                        self.feasible_ps = map.cells_from_sector(*sector);
                        self.feasible_ps.retain(
                            |pos| map.is_water(*pos)
                        );
                    } else if self.feasible_ps.len() != 1 {
                        self.feasible_ps.retain(
                            |pos| map.belongs_to_sector(pos, *sector)
                        )
                    }
                },
                OppAction::Torpedo{target} => {
                    // TODO: account for "you can also damage yourself with a torpedo"
                    if self.feasible_ps.is_empty() {
                        self.feasible_ps = map.cells_within_range(*target, 4)
                    } else if self.feasible_ps.len() != 1 {
                        self.feasible_ps.retain(
                            |pos| map.are_within_range(*pos, *target, 4)
                        )
                    }
                },
            }

            eprintln!("{:?}", self.feasible_ps)
        }
    }
}

impl Map {
    fn cells_from_sector(&self, sector_id: usize) -> Vec<Coord> {
        let (min_x, min_y) = self.sector_addr(sector_id);
        Coord::range(min_x, min_x + 4, min_y, min_y + 4)
    }

    fn sector_addr(&self, sector_id: usize) -> (usize, usize) {
        // 5x5 cells/sector, sectors numbered 1..9 first horizontal then vertical
        let sector_addr = sector_id - 1;
        let min_y = (sector_addr / 3) * 5;
        let min_x = (sector_addr % 3) * 5;

        (min_x, min_y)
    }

    fn belongs_to_sector(&self, pos: &Coord, sector_id: usize) -> bool {
        let (min_x, min_y) = self.sector_addr(sector_id);
        min_x <= pos.x && pos.x <= min_x + 4
            && min_y <= pos.y && pos.y <= min_y + 4
    }

    fn is_viable_move(&self, pos: Coord, dir: char) -> bool {
        let dest = pos._move(dir);
        self.is_within_bounds(dest) && self.is_water(dest)
    }

    // invariant: a, b are coords to water cells
    fn are_within_range(&self, a: Coord, b: Coord, range: usize) -> bool {
        if a == b {
            return true
        } else if range == 0 || !a.le_distance(b, range) {
            return false
        }

        // NICE: maybe keep track of visited?
        //       - with an inner method (_are...) and a shared visited vector.
        //       - not very critical with range=4 & Coord::le_distance()
        for coord in &a.neighbors() {
            if self.is_within_bounds(*coord) && self.is_water(*coord) {
                if self.are_within_range(*coord, b, range-1) {
                    return true
                }
            }
        }

        false
    }

    fn cells_within_range(&self, pos: Coord, range: usize) -> Vec<Coord> {
        let mut cells = Vec::with_capacity(
            // max possible number of cells within this range of pos
            (range+1).pow(2) + range.pow(2)
        );
        cells.push(pos);
        // BFS
        let mut pre_iter_start = 0;
        for _ in 1..=range {
            let mut new_neighbors = Vec::with_capacity(
                // 4-1 neighbors/cell (one neighbor was stepped into in the previous step)
                // +1 for the root case, which won't have any visited neighbors
                3* cells[pre_iter_start..].len() + 1
            );
            for pos in &cells[pre_iter_start..] {
                let neighbors = pos.neighbors().into_iter().filter(|coord| {
                    self.is_within_bounds(*coord) && self.is_water(*coord)
                        && ! cells[pre_iter_start..].contains(coord)
                });
                for neighbor in neighbors {
                    if ! new_neighbors.contains(&neighbor) {
                            new_neighbors.push(neighbor)
                    }
                }
            }
            pre_iter_start = cells.len();
            cells.append(&mut new_neighbors);
        }

        cells
    }
}

impl Coord {
    fn range(min_x: usize, max_x: usize, min_y: usize, max_y: usize) -> Vec<Coord> {
        let mut range = Vec::new();
        for y in min_y..=max_y {
            for x in min_x..=max_x {
                range.push(Coord{ x, y });
            }
        }

        range
    }

    fn _move(&self, dir: char) -> Coord {
        match dir {
            'N' => Coord { y: self.y - 1, ..*self },
            'E' => Coord { x: self.x + 1, ..*self },
            'S' => Coord { y: self.y + 1, ..*self },
            'W' => Coord { x: self.x - 1, ..*self },
            _ => panic!("Coord::_move(): not a direction char?")
        }
    }

    fn neighbors(&self) -> Vec<Coord> {
        vec![
            Coord{ x: self.x - 1, ..(*self) },
            Coord{ x: self.x + 1, ..(*self) },
            Coord{ y: self.y - 1, ..(*self) },
            Coord{ y: self.y + 1, ..(*self) }
        ]
    }

    fn le_distance(&self, other: Coord, distance: usize) -> bool {
        let distance_x = (self.x as isize - other.x as isize).abs();
        let distance_y = (self.y as isize - other.y as isize).abs();

        distance_x + distance_y <= distance as isize
    }
}
