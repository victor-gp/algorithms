fn main() {
    let mut timer = Timer::new();

    let mut global = read_starting_info();
    timer.get_turn_pointer(&global);
    let mut me = Me::new();
    let mut opp = Opponent::new();

    // 1st turn: choose a starting position
    println!("{}", global.initial_pos());

    timer.setup_stop();

    loop {
        timer.start();

        global.turn += 1;
        read_turn_info(&global, &mut me, &mut opp);

        let action_seq = me.next_actions(&global, &opp);
        me.register_actions(&action_seq);

        println!("{}", action_seq);

        timer.stop();
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

#[derive(Copy, Clone, PartialEq)]
enum Action {
    Move { dir: char },
    Surface,
    Torpedo { target: Coord },
    // Msg { message: &'static str },
}

enum OppAction {
    Move { dir: char },
    Surface { sector: usize },
    Torpedo { target: Coord },
}

struct Map {
    width: usize,
    height: usize,
    grid: Vec<Vec<Cell>>,
    water: Vec<Coord>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
// agnostic of Map, doesnt't care about bounds or Water/Land
struct Coord {
    x: usize,
    y: usize
}

enum Cell { Water, Land }

use std::time::{Duration, Instant};

struct Timer {
    start: Instant,
    turns_acc: Duration,
    turn_ptr: *const usize
}

use std::io;

macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
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

    fn is_water_cell(&self) -> bool {
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

impl fmt::Debug for OppAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Move{dir} => write!(f, "MOVE {}", dir),
            Self::Surface{sector} => write!(f, "SURFACE {}", sector),
            Self::Torpedo{target} => write!(f, "TORPEDO {}", target),
        }
    }
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
            _ => {
                eprintln!("OppAction::from_str: could not parse string \"{}\"", s);
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

use std::ptr;

impl Timer {
    fn new() -> Timer {
        Timer {
            start: Instant::now(),
            turns_acc: Duration::new(0,0),
            turn_ptr: ptr::null()
        }
    }

    fn start(&mut self) {
        self.start = Instant::now();
    }

    fn setup_stop(&self) {
        eprintln!("Response time: {} ms", self.start.elapsed().as_millis())
    }

    fn stop(&mut self) {
        let elapsed = self.start.elapsed();
        self.turns_acc += elapsed;
        let turn = unsafe { self.turn_ptr.as_ref().unwrap() };
        let real_turn = (turn -1) as f32;
        let mean = self.turns_acc.div_f32(real_turn);

        eprintln!("Response time: {} ms / Mean: {} ms",
            elapsed.as_millis(), mean.as_millis());
    }

    fn get_turn_pointer(&mut self, global: &Global) {
        self.turn_ptr = &global.turn;
    }
}

impl Map {
    fn new(width: usize, height: usize, grid: Vec<Vec<Cell>>) -> Map {
        let water = Map::water_positions(&grid);

        Map { width, height, grid, water }
    }

    // internal, Map methods only!
    fn cell_at(&self, coord: Coord) -> &Cell {
        &self.grid[coord.y][coord.x]
    }

    fn is_within_bounds(&self, coord: Coord) -> bool {
        // no need for 0 <= coords cause usize overflows on -1
        coord.x < self.width
            && coord.y < self.height
    }

    // use this to skip the is_within_bounds() check
    fn is_water(&self, coord: Coord) -> bool {
        self.is_within_bounds(coord)
            && self.cell_at(coord).is_water_cell()
    }

    fn is_viable_move(&self, pos: Coord, dir: char) -> bool {
        let dest = pos.after_move(dir);
        self.is_water(dest)
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

    // TODO: precalculate all distances at Map::new() time
    // cond: a, b are coords to water cells
    fn distance(&self, a: &Coord, b: &Coord) -> Option<usize> {
        let mut visited = vec![vec![false; self.width]; self.height];
        self._distance(a, b, &mut visited)
    }

    // invariant: a, b are coords to water cells
    fn _distance(&self, a: &Coord, b: &Coord, visited: &mut Vec<Vec<bool>>) -> Option<usize> {
        if a == b { return Some(0) }

        for coord in a.neighbors_by_direction(b).iter()
                      .filter(|&coord| self.is_water(*coord)) {
            if visited[coord.y][coord.x] { continue }
            visited[coord.y][coord.x] = true;

            match self._distance(coord, b, visited) {
                Some(distance) => return Some(distance +1),
                None => continue
            }
        }
        None
    }

    // invariant: a, b are coords to water cells
    fn are_within_distance(&self, a: Coord, b: Coord, distance: usize) -> bool {
        if a == b {
            return true
        } else if distance == 0 || a.min_distance(b) > distance {
            return false
        }

        for coord in a.neighbors_by_direction(&b) {
            if self.is_water(coord) && self.are_within_distance(coord, b, distance-1) {
                return true
            }
        }
        false
    }

    // water cells_only, land cells are stepped around
    fn cells_within_distance(&self, pos: Coord, distance: usize) -> Vec<Coord> {
        let mut cells = Vec::with_capacity(
            // max possible number of cells within this distance of pos
            (distance+1).pow(2) + distance.pow(2)
        );
        cells.push(pos);
        // BFS
        let mut pre_iter_start = 0;
        for _ in 1..=distance {
            let mut new_neighbors = Vec::with_capacity(
                // 4-1 neighbors/cell (one neighbor was stepped into in the previous step)
                // +1 for the root case, which won't have any visited neighbors
                3* cells[pre_iter_start..].len() + 1
            );
            for pos in &cells[pre_iter_start..] {
                let neighbors = pos.neighbors().into_iter().filter(|coord| {
                    self.is_water(*coord) && ! cells[pre_iter_start..].contains(coord)
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

    fn belongs_to_sector(&self, pos: &Coord, sector_id: usize) -> bool {
        let (min_x, min_y) = self.sector_addr(sector_id);
        min_x <= pos.x && pos.x <= min_x + 4
            && min_y <= pos.y && pos.y <= min_y + 4
    }

    fn water_cells_from_sector(&self, sector_id: usize) -> Vec<Coord> {
        let (min_x, min_y) = self.sector_addr(sector_id);
        let cells_from_sector = Coord::range(min_x, min_x + 4, min_y, min_y + 4);

        cells_from_sector.into_iter().filter(
            |coord| self.cell_at(*coord).is_water_cell()
        ).collect()
    }

    // sectors numbered 1 to 9, 5x5 cells/sector, first horizontal then vertical
    fn sector_addr(&self, sector_id: usize) -> (usize, usize) {
        let sector_addr = sector_id - 1;
        let min_y = (sector_addr / 3) * 5;
        let min_x = (sector_addr % 3) * 5;

        (min_x, min_y)
    }
}

impl Coord {
    fn after_move(&self, dir: char) -> Coord {
        match dir {
            'N' => Coord { y: self.y - 1, ..*self },
            'E' => Coord { x: self.x + 1, ..*self },
            'S' => Coord { y: self.y + 1, ..*self },
            'W' => Coord { x: self.x - 1, ..*self },
            _ => panic!("Coord::after_move(): not a direction char?")
        }
    }

    fn neighbors(&self) -> Vec<Coord> {
        vec![
            self.after_move('N'),
            self.after_move('E'),
            self.after_move('S'),
            self.after_move('W')
        ]
    }

    fn neighbors_by_direction(&self, reference_point: &Coord) -> Vec<Coord> {
        let mut neighbors = self.neighbors();
        neighbors.sort_by(|a, b| {
            let dist_a = a.min_distance(*reference_point);
            let dist_b = b.min_distance(*reference_point);

            dist_a.partial_cmp(&dist_b).unwrap()
        });

        neighbors
    }

    fn range(min_x: usize, max_x: usize, min_y: usize, max_y: usize) -> Vec<Coord> {
        let mut range = Vec::new();
        for y in min_y..=max_y {
            for x in min_x..=max_x {
                range.push(Coord{ x, y });
            }
        }

        range
    }

    // min (possible) distance, doesn't account for cells being Water/Land
    fn min_distance(&self, other: Coord) -> usize {
        let distance_x = (self.x as isize - other.x as isize).abs();
        let distance_y = (self.y as isize - other.y as isize).abs();

        (distance_x + distance_y) as usize
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

    fn is_position_known(&self) -> bool {
        self.feasible_ps.len() == 1
    }

    // cond: position is known
    fn position(&self) -> Coord {
        self.feasible_ps[0]
    }

    fn is_position_tracked(&self) -> bool {
        ! self.feasible_ps.is_empty()
    }

    fn analyze_actions(&mut self, map: &Map, actions: &Vec<OppAction>) {
        for action in actions {
            match action {
                OppAction::Move{dir} => {
                    if self.is_position_tracked() {
                        self.feasible_ps = self.feasible_ps.iter().filter_map(|pos| {
                            if map.is_viable_move(*pos, *dir) {
                                Some(pos.after_move(*dir))
                            } else { None }
                        }).collect()
                    }
                },
                OppAction::Surface{sector} => {
                    if ! self.is_position_tracked() {
                        self.feasible_ps = map.water_cells_from_sector(*sector);
                    } else if ! self.is_position_known() {
                        self.feasible_ps.retain(
                            |pos| map.belongs_to_sector(pos, *sector)
                        )
                    }
                },
                OppAction::Torpedo{target} => {
                    // TODO: account for "you can also damage yourself with a torpedo"
                    //       note: torpedo affectation range includes diagonals
                    if ! self.is_position_tracked() {
                        self.feasible_ps = map.cells_within_distance(*target, 4)
                    } else if ! self.is_position_known() {
                        self.feasible_ps.retain(
                            |pos| map.are_within_distance(*pos, *target, 4)
                        )
                    }
                },
            }

            eprintln!("{:?}", self.feasible_ps)
        }
    }

    // TODO: traceback moves when I init feasible_ps
    //       with an action/move history and Coord.before_move()
}

use rand::seq::IteratorRandom;

impl Global {
    fn initial_pos(&self) -> &Coord {
        self.map.water.iter()
            .choose(&mut rand::thread_rng()).unwrap()
    }
}

impl Me {
    fn next_actions(&self, global: &Global, opp: &Opponent) -> ActionSeq {
        let mut action_seq = ActionSeq::new();
        let viable_moves = self.viable_moves(&global.map);
        let have_to_surface = viable_moves.is_empty();
        let should_fire = self.should_fire(&opp, &global.map);

        if have_to_surface {
            action_seq.push(Action::Surface)
        }
        /*if may_fire {
            let possible_torpedoes = Action.torpedoes_impacting(&opp.position());
            let combinations = ActionSeq.viable_combinations(&viable_moves, &possible_torpedoes, &self, &global.map);
            let scores = combinations.iter().map(|&c| c.score());
            // score params: my_lives, their_lives, my_pos

            let (index, max_score) = scores.enumerate().max_by(|&(_, score)| score);
            action_seq.push(combinations[index])

        }*/
        if should_fire {
            action_seq.push(Action::Torpedo{ target: opp.position() })
        }
        if !have_to_surface {
            action_seq.push(viable_moves[0])
        }

        action_seq
    }

    fn viable_moves(&self, map: &Map) -> Vec<Action> {
        let directions = ['N', 'E', 'S', 'W'];
        let destinations = directions.iter().map(|&dir| self.pos.after_move(dir));

        directions.iter().zip(destinations).filter_map(|dir_dest| {
            let (&dir, dest) = dir_dest;
            if map.is_water(dest) && ! self.visited.contains(&dest) {
                Some(Action::Move{dir})
            }
            else { None }
        }).collect()
    }

    fn may_fire(&self, opp: &Opponent, map: &Map) -> bool {
        self.torpedo_cooldown == 0
            && opp.is_position_known()
            && match map.distance(&self.pos, &opp.position()) {
                // torpedo range (4) + impact area (1) + approaching move (1)
                // FIXME: impact area may be 2, counting diagonals...
                Some(distance) => distance <= 6,
                None => false
            }
    }

    fn register_actions(&mut self, actions: &ActionSeq) {
        if actions.contains(&Action::Surface) {
            self.visited.clear()
        }
        // torpedo charge is included in turn info
    }

    fn should_fire(&self, opp: &Opponent, map: &Map) -> bool {
        self.torpedo_cooldown == 0
            && opp.is_position_known()
            && match map.distance(&self.pos, &opp.position()) {
                // torpedo impact area = target + 1 including diagonals
                // torpedo range = 4
                Some(distance) => 2 <= distance && distance <= 4,
                None => false
            }
    }
}

struct ActionSeq(Vec<Action>);
// NICE: make this a generic type to contain both action types (mainly for IO purposes)
//       with an IOAction trait (Display, Debug, from_str) implemented by the two types

use std::ops::{Deref, DerefMut};

impl Deref for ActionSeq {
    type Target = Vec<Action>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ActionSeq {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// NICE: try adversarial output (whitespace, inCoNsISteNt cASE)
impl fmt::Display for ActionSeq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let seq_string = self.iter().map(|action| {
            format!("{}", action)
        }).collect::< Vec<String> >().join("|");

        write!(f, "{}", seq_string)
    }
}

impl fmt::Debug for ActionSeq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl ActionSeq {
    fn new() -> Self {
        ActionSeq(Vec::new())
    }
}
