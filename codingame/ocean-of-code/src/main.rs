fn main() {
    let mut timer = Timer::new();

    let mut global = read_starting_info();
    timer.get_turn_pointer(&global);
    let mut me = Me::new();
    let mut them = Them::new();

    // 1st turn: choose a starting position
    println!("{}", global.initial_pos());

    timer.setup_stop();

    loop {
        timer.start();

        global.turn += 1;
        read_turn_info(&global, &mut me, &mut them);

        let action_seq = me.next_actions(&global, &them);
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
    torpedo_cooldown: usize,
    sonar_cooldown: usize,
    // silence_cooldown: usize,
    // mine_cooldown: usize,
    last_sonar_sector: usize,
}

struct Them {
    lives: usize,
    pos_candidates: Vec<Coord>,
    // event_history: Vec<TheirEvent>,
    // cooldowns
}

#[derive(PartialEq)]
enum Action {
    Move { dir: char, charge_device: &'static str },
    Surface,
    Torpedo { target: Coord },
    Sonar { sector: usize },
    // TODO: SILENCE
}

// devices for Action::Move.charge_device
const TORPEDO: &str = "TORPEDO";
const SONAR: &str = "SONAR";

enum TheirEvent {
    Move { dir: char },
    Surface { sector: usize },
    Torpedo { target: Coord },
    Sonar { sector: usize },
    Silence,
    MySonar { sector: usize, success: bool },
    // LifeLoss { usize: lives }
}

struct ActionSeq(Vec<Action>);
// NICE: make this a generic type to contain both action types (mainly for IO purposes)
//       with an IOAction trait (Display, Debug, from_str) implemented by the two types

struct Map {
    width: usize,
    height: usize,
    grid: Vec<Vec<Cell>>,
    water: Vec<Coord>,
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
// agnostic of Map, doesnt't care about bounds or Water/Land
struct Coord {
    x: usize,
    y: usize
}

const DIRECTIONS: [char; 4] = ['N', 'E', 'S', 'W'];

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

    Global::new(map, my_id)
}

#[allow(unused_variables)]
fn read_turn_info(global: &Global, me: &mut Me, them: &mut Them) {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let inputs = input_line.split(" ").collect::<Vec<_>>();
    let x = parse_input!(inputs[0], usize);
    let y = parse_input!(inputs[1], usize);
    let my_life = parse_input!(inputs[2], usize);
    let opp_life = parse_input!(inputs[3], usize);
    let torpedo_cooldown = parse_input!(inputs[4], usize);
    let sonar_cooldown = parse_input!(inputs[5], usize);
    let silence_cooldown = parse_input!(inputs[6], usize);
    let mine_cooldown = parse_input!(inputs[7], i32);
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let sonar_result = input_line.trim().to_string();
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let opponent_orders = input_line.trim_matches('\n').to_string();

    if global.turn > 2 {
        me.visited.push(me.pos.clone());
    }
    me.pos = Coord{x, y};
    me.lives = my_life;
    them.lives = opp_life;
    me.torpedo_cooldown = torpedo_cooldown;
    me.sonar_cooldown = sonar_cooldown;

    if global.turn > 2 || ! global.me_first {
        let mut their_events = TheirEvent::action_seq_from_str(&opponent_orders);
        if let Some(sonar_event) = me.parse_sonar_result(&sonar_result) {
            their_events.insert(0, sonar_event)
        }
        them.analyze_events(their_events, &global.map);
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
            Self::Move{dir, charge_device} => write!(f, "MOVE {} {}", dir, charge_device),
            Self::Surface => write!(f, "SURFACE"),
            Self::Torpedo{target} => write!(f, "TORPEDO {}", target),
            Self::Sonar{sector}  => write!(f, "SONAR {}", sector),
        }
    }
}

impl fmt::Debug for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Debug for TheirEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Move{dir} => write!(f, "MOVE {}", dir),
            Self::Surface{sector} => write!(f, "SURFACE {}", sector),
            Self::Torpedo{target} => write!(f, "TORPEDO {}", target),
            Self::Sonar{sector} => write!(f, "SONAR {}", sector),
            Self::Silence => write!(f, "SILENCE"),
            Self::MySonar{sector, success} =>
                write!(f, "(MySonar {} {})", sector, success),
        }
    }
}

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

impl TheirEvent {
    fn from_action_str(s: &str) -> Option<Self> {
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
            "SONAR" => {
                let sector = parse_input!(tokens.next()?, usize);
                Some( Self::Sonar{ sector: sector } )
            },
            "SILENCE" => Some( Self::Silence ),
            _ => {
                eprintln!("TheirEvent::from_str: could not parse string \"{}\"", s);
                None
            }
        }
    }

    fn action_seq_from_str(action_seq: &str) -> Vec<Self> {
        action_seq
            .split("|")
            .filter_map(|s| Self::from_action_str(s))
            .collect()
    }

    #[allow(dead_code)]
    fn string_from_seq(event_seq: Vec<Self>) -> String {
        event_seq
            .iter()
            .map(|action| {format!("{:?}", action)})
            .collect::<Vec<String>>()
            .join("|")
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

impl Action {
    fn new_move(dir: char) -> Action {
        Action::Move { dir , charge_device: TORPEDO }
    }

    fn but_charge(&self, device: &'static str) -> Action {
        if let Action::Move{ dir, charge_device: _} = self {
            Action::Move { dir: *dir, charge_device: device }
        }
        else {
            panic!("used Action::but_charge on \"{}\", should be a Move", self)
        }
    }
}

impl ActionSeq {
    fn new() -> Self {
        ActionSeq(Vec::new())
    }
}

impl Map {
    fn new(width: usize, height: usize, grid: Vec<Vec<Cell>>) -> Map {
        let water = Map::water_positions(&grid);

        Map { width, height, grid, water }
    }

    // internal, Map methods only!
    fn cell_at(&self, coord: &Coord) -> &Cell {
        &self.grid[coord.y][coord.x]
    }

    fn is_within_bounds(&self, coord: &Coord) -> bool {
        // no need for 0 <= coords cause usize overflows on -1
        coord.x < self.width
            && coord.y < self.height
    }

    // use this to skip the is_within_bounds() check
    fn is_water(&self, coord: &Coord) -> bool {
        self.is_within_bounds(&coord)
            && self.cell_at(&coord).is_water_cell()
    }

    fn is_viable_move(&self, pos: &Coord, dir: char) -> bool {
        let dest = pos.after_move(dir);
        self.is_water(&dest)
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

    // TODO: try to precalculate all distances at Map::new() time
    // cond: a, b are coords to water cells
    fn distance(&self, a: &Coord, b: &Coord) -> Option<usize> {
        let mut visited = vec![vec![false; self.width]; self.height];
        self._distance(a, b, &mut visited)
    }

    // invariant: a, b are coords to water cells
    fn _distance(&self, a: &Coord, b: &Coord, visited: &mut Vec<Vec<bool>>) -> Option<usize> {
        if a == b { return Some(0) }

        for coord in a.neighbors_by_direction(b).iter()
                      .filter(|&coord| self.is_water(&coord)) {
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
    fn are_within_distance(&self, a: &Coord, b: &Coord, distance: usize) -> bool {
        if a == b {
            return true
        } else if distance == 0 || a.min_distance(&b) > distance {
            return false
        }

        for coord in a.neighbors_by_direction(&b) {
            if self.is_water(&coord) && self.are_within_distance(&coord, b, distance-1) {
                return true
            }
        }
        false
    }

    // water cells_only, land cells are stepped around
    fn cells_within_distance(&self, pos: &Coord, distance: usize) -> Vec<Coord> {
        let mut cells = Vec::with_capacity(
            // max possible number of cells within this distance of pos
            (distance+1).pow(2) + distance.pow(2)
        );
        cells.push(pos.clone());
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
                    self.is_water(coord) && ! cells[pre_iter_start..].contains(coord)
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

    // invariant: pos refers to a Water cell
    // return: n cells towards dir, or until leaving water, in order
    fn cells_along(&self, pos: &Coord, dir: char, n: usize) -> Vec<Coord> {
        if n == 0 { return Vec::new() }

        let next = pos.after_move(dir);
        if self.is_water(&next) {
            let mut ret = self.cells_along(&next, dir, n-1);
            ret.insert(0, next);
            ret
        } else {
            Vec::new()
        }
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
            |coord| self.cell_at(coord).is_water_cell()
        ).collect()
    }

    // sectors numbered 1 to 9, 5x5 cells/sector, first horizontal then vertical
    fn sector_addr(&self, sector_id: usize) -> (usize, usize) {
        let sector_addr = sector_id - 1;
        let min_y = (sector_addr / 3) * 5;
        let min_x = (sector_addr % 3) * 5;

        (min_x, min_y)
    }

    fn sector_from(&self, pos: &Coord) -> usize {
        let vertical = pos.y / 5;
        let horizontal = pos.x / 5;

        vertical*3 + horizontal + 1
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
        DIRECTIONS
            .iter()
            .map(|&dir| self.after_move(dir))
            .collect()
    }

    // neighbors sorted by their min_distance to reference_point (the direction)
    fn neighbors_by_direction(&self, reference_point: &Coord) -> Vec<Coord> {
        let mut neighbors = self.neighbors();
        neighbors.sort_by(|a, b| {
            let dist_a = a.min_distance(&reference_point);
            let dist_b = b.min_distance(&reference_point);

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
    fn min_distance(&self, other: &Coord) -> usize {
        let distance_x = (self.x as isize - other.x as isize).abs();
        let distance_y = (self.y as isize - other.y as isize).abs();

        (distance_x + distance_y) as usize
    }
}

use rand::seq::IteratorRandom;

impl Global {
    fn new(map: Map, my_id: usize) -> Global {
        Global {
            map,
            turn: 1,
            game_turns: 300,
            me_first: my_id == 0
        }
    }
    fn initial_pos(&self) -> &Coord {
        self.map.water.iter()
            .choose(&mut rand::thread_rng()).unwrap()
    }
}

impl Me {
    fn new() -> Me {
        Me {
            lives: 0,
            pos: Coord{x: 0, y: 0},
            visited: Vec::new(),
            torpedo_cooldown: 0,
            sonar_cooldown: 0,
            last_sonar_sector: 0,
        }
    }

    fn viable_moves(&self, map: &Map) -> Vec<Action> {
        // Coord.neighbors() uses the same order as DIRECTIONS
        let destinations = self.pos.neighbors();

        DIRECTIONS.iter().zip(destinations).filter_map(|dir_dest| {
            let (&dir, dest) = dir_dest;
            if map.is_water(&dest) && ! self.visited.contains(&dest) {
                Some(Action::new_move(dir))
            }
            else { None }
        }).collect()
    }

    fn register_actions(&mut self, actions: &ActionSeq) {
        for action in &actions[..] {
            if let &Action::Surface = action {
                self.visited.clear()
            } else if let &Action::Sonar{sector} = action {
                self.last_sonar_sector = sector
            }
        }
        // device dis/charges are ack'd in the turn info (cooldowns)
    }

    fn should_fire(&self, them: &Them, map: &Map) -> bool {
        self.torpedo_cooldown == 0
            && them.is_position_known()
            && match map.distance(&self.pos, &them.position()) {
                // torpedo impact area = target + 1 including diagonals
                // torpedo range = 4
                Some(distance) => 2 <= distance && distance <= 4,
                None => false
            }
    }

    fn device_to_charge(&self, them: &Them) -> &'static str {
        let sonar_discharged = self.sonar_cooldown > 0;
        let torpedo_discharged = self.torpedo_cooldown > 0;
        // NICE: consider the CD amount
        // NICE: take into account the CDs after the incoming action_seq

        if ! sonar_discharged {
            TORPEDO
        }
        else if ! torpedo_discharged {
            SONAR
        }
        else if them.is_position_narrow() { // NICE: && are we close enough?
            TORPEDO
        }
        else {
            SONAR
        }
    }

    fn should_sonar(&self, them: &Them) -> bool {
        if self.sonar_cooldown > 0 { return false }
        let sonar_score = them.sonar_potential();

        sonar_score > 0.5
    }
}

use std::iter;
use std::collections::HashMap;

impl Them {
    fn new() -> Them {
        Them {
            lives: 0,
            pos_candidates: Vec::new(),
        }
    }

    fn is_position_known(&self) -> bool {
        self.pos_candidates.len() == 1
    }

    // cond: position is known
    fn position(&self) -> Coord {
        self.pos_candidates[0].clone()
    }

    fn is_position_tracked(&self) -> bool {
        ! self.pos_candidates.is_empty()
    }

    // as in "narrowed down"
    fn is_position_narrow(&self) -> bool {
        1 < self.pos_candidates.len()
            && self.pos_candidates.len() <= 25
    }

    fn analyze_events(&mut self, events: Vec<TheirEvent>, map: &Map) {
        for event in &events {
            match event {
                TheirEvent::Move{dir} => self.analyze_move(*dir, map),
                TheirEvent::Surface{sector} => self.analyze_surface(*sector, map),
                TheirEvent::Torpedo{target} => self.analyze_torpedo(target, map),
                TheirEvent::Sonar{sector: _} => (),
                TheirEvent::Silence => self.analyze_silence(map),
                TheirEvent::MySonar{sector, success} =>
                    self.analyze_my_sonar(*sector, *success, map),
            }
            eprintln!("{}: {:?}", self.pos_candidates.len(), self.pos_candidates)
        }
    }

    fn analyze_move(&mut self, dir: char, map: &Map) {
        if self.is_position_tracked() {
            self.pos_candidates =
                self.pos_candidates.iter()
                .filter_map(|pos| {
                    if map.is_viable_move(pos, dir) {
                        Some(pos.after_move(dir))
                    } else { None }
                })
                .collect()
        }
    }

    fn analyze_surface(&mut self, sector: usize, map: &Map) {
        if ! self.is_position_tracked() {
            self.pos_candidates = map.water_cells_from_sector(sector);
        } else if ! self.is_position_known() {
            self.pos_candidates.retain(
                |pos| map.belongs_to_sector(pos, sector)
            )
        }
    }

    fn analyze_torpedo(&mut self, target: &Coord, map: &Map) {
        if ! self.is_position_tracked() {
            self.pos_candidates = map.cells_within_distance(&target, 4)
        } else if ! self.is_position_known() {
            self.pos_candidates.retain(
                |pos| map.are_within_distance(pos, &target, 4)
            )
        }
        // NICE: account for "you can also damage yourself with a torpedo"
        //       note: torpedo affectation range includes diagonals
    }

    fn analyze_silence(&mut self, map: &Map) {
        let max_sonar_dist = 4;
        self.pos_candidates = self.pos_candidates.iter().flat_map(|pos| {
            DIRECTIONS.iter().flat_map(move |&dir|
                map.cells_along(&pos.clone(), dir, max_sonar_dist)
                //NICE: I could use them.visited here, to drop paths through visited cells
            ).chain(iter::once(pos.clone()))
        }).collect();
        self.pos_candidates.sort_unstable();
        self.pos_candidates.dedup();

        // rather than them.visited (only works when position_is_known),
        //  I should just spam traceback (until latest Surface, returns bool if feasible),
        //  and visited would be just another internal parameter of traceback
    }

    fn analyze_my_sonar(&mut self, sector: usize, success: bool, map: &Map) {
        if success {
            self.analyze_surface(sector, map)
        } else if self.is_position_tracked() && ! self.is_position_known() {
            self.pos_candidates.retain(
                |pos| ! map.belongs_to_sector(pos, sector)
            )
        }
    }

    fn sonar_potential(&self) -> f32 {
        // TODO: how much will pos_candidates decrease (candidates in sector_to_sonar)
        // NICE: bring Them.silence_cooldown into the fold
        let is_good =
            self.is_position_tracked()
            && ! self.is_position_known()
            && self.is_position_narrow();

        if is_good { 1. }
        else { 0. }
    }

    fn sector_to_sonar(&self, map: &Map) -> usize {
        let sectors = self.pos_candidates.iter().map(|pos| map.sector_from(pos));
        let mut sector_counts = HashMap::new();
        for sector in sectors {
            let sector_count = sector_counts.entry(sector).or_insert(1);
            *sector_count += 1;
        }
        let max = sector_counts
                  .iter()
                  .max_by(|a, b| a.1.cmp(&b.1))
                  .map(|(s, _)| s);
        match max {
            Some(&sector) => sector,
            None => 1
        }
    }

    // TODO: traceback moves when I init pos_candidates
    //       with an action/move history and Coord.before_move()
}

impl Me {
    fn next_actions(&self, global: &Global, them: &Them) -> ActionSeq {
        let mut action_seq = ActionSeq::new();
        let viable_moves = self.viable_moves(&global.map);
        let have_to_surface = viable_moves.is_empty();
        let should_fire = self.should_fire(&them, &global.map);
        let should_sonar = self.should_sonar(&them);

        if have_to_surface {
            action_seq.push(Action::Surface)
        }
        /*if may_fire {
            let possible_torpedoes = Action.torpedoes_impacting(&them.position());
            let combinations = ActionSeq.viable_combinations(&viable_moves, &possible_torpedoes, &self, &global.map);
            let scores = combinations.iter().map(|&c| c.score());
            // score params: my_lives, their_lives, my_pos

            let (index, max_score) = scores.enumerate().max_by(|&(_, score)| score);
            action_seq.push(combinations[index])

        }*/
        if should_fire {
            action_seq.push(Action::Torpedo{ target: them.position() })
        }
        if !have_to_surface {
            // TODO: better pathing, including Silence
            let device = self.device_to_charge(them);
            action_seq.push(
                viable_moves[0].but_charge(device)
            )
        }
        if should_sonar {
            let sector = them.sector_to_sonar(&global.map);
            action_seq.push(Action::Sonar { sector })
        }

        action_seq
    }

    fn parse_sonar_result(&self, result: &str) -> Option<TheirEvent> {
        if result != "NA" {
            let success = result == "Y";
            Some(TheirEvent::MySonar{ sector: self.last_sonar_sector, success })
        } else {
            None
        }
    }
}
