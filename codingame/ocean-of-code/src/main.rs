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
    visited: CoordSet,
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
    // MyTorpedo { target: Coord } at beginning of turn,
    // LifeLoss { usize: lives, torpedo_targets: Vec<Coord> } at end of turn,
    // SilenceDivergence { pre_candidates } at/replacing every Silence,
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

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
// agnostic of Map, doesnt't care about bounds or Water/Land
struct Coord {
    x: usize,
    y: usize
}

const DIRECTIONS: [char; 4] = ['N', 'E', 'S', 'W'];

type CoordSet = HashSet<Coord>;

enum Cell { Water, Land }

struct Timer {
    start: Instant,
    turns_acc: Duration,
    turn_ptr: *const usize
}

use std::io;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::time::{Duration, Instant};
use std::ptr;
use rand::seq::IteratorRandom;
use std::iter;
use std::collections::{HashMap, HashSet};
use std::cmp;

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
        me.visited.insert(me.pos.clone());
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
            Self::Surface => {
                eprintln!("mine:surface");
                write!(f, "SURFACE")
            },
            Self::Torpedo{target} => {
                eprintln!("mine:torpedo");
                write!(f, "TORPEDO {}", target)
            },
            Self::Sonar{sector} => {
                eprintln!("mine:sonar");
                write!(f, "SONAR {}", sector)
            },
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
    #[allow(dead_code)]
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
        // BFS
        cells.push(pos.clone());
        let mut pre2_iter_start = 0;
        let mut pre_iter_start = 0;
        for _ in 1..=distance {
            let mut new_neighbors = Vec::with_capacity(
                // 4-1 neighbors/cell (one neighbor was stepped into in the previous step)
                // +1 for the root case, which won't have any visited neighbors
                3* cells[pre_iter_start..].len() + 1
            );
            for pos in &cells[pre_iter_start..] {
                let neighbors = pos.neighbors().into_iter().filter(|coord| {
                    self.is_water(coord) && ! cells[pre2_iter_start..].contains(coord)
                });
                for neighbor in neighbors {
                    if ! new_neighbors.contains(&neighbor) {
                        new_neighbors.push(neighbor)
                    }
                }
            }
            pre2_iter_start = pre_iter_start;
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

    // cond: sector_id in [1,9]
    fn water_cells_from_sector(&self, sector_id: usize) -> Vec<Coord> {
        let (min_x, min_y) = self.sector_addr(sector_id);
        let cells_from_sector = Coord::range(min_x, min_x + 4, min_y, min_y + 4);

        cells_from_sector
            .into_iter()
            // if sector is correct, cells_from_sector are all within bounds
            .filter(|coord| self.cell_at(coord).is_water_cell())
            .collect()
    }

    // sectors numbered 1 to 9, 5x5 cells/sector, first horizontal then vertical
    fn sector_addr(&self, sector_id: usize) -> (usize, usize) {
        let sector_addr = sector_id - 1;
        let min_y = (sector_addr / 3) * 5;
        let min_x = (sector_addr % 3) * 5;

        (min_x, min_y)
    }

    // cond: pos is within bounds
    fn sector_from(&self, pos: &Coord) -> usize {
        let vertical = pos.y / 5;
        let horizontal = pos.x / 5;

        vertical*3 + horizontal + 1
    }

    // water cells impacted by a torpedo fired at torpedo_target
    fn area_of_effect(&self, torpedo_target: &Coord) -> CoordSet {
        torpedo_target
            .neighbors_with_diagonals()
            .into_iter()
            .filter(|coord| self.is_water(&coord))
            .chain(iter::once(torpedo_target.clone()))
            .collect()
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

    fn neighbors_with_diagonals(&self) -> Vec<Coord> {
        let mut neighbors = self.neighbors();
        neighbors.append(&mut vec![
            Coord { x: self.x -1, y: self.y -1 },
            Coord { x: self.x +1, y: self.y -1 },
            Coord { x: self.x +1, y: self.y +1 },
            Coord { x: self.x -1, y: self.y +1 }
        ]);

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
            visited: CoordSet::new(),
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

    fn device_to_charge(&self, them: &Them) -> &'static str {
        let sonar_discharged = self.sonar_cooldown > 0;
        let torpedo_discharged = self.torpedo_cooldown > 0;
        // TODO: if we're close, Torpedo or Silence (prioritize by charges to ready)
        //       or just use best_shot().score and sonar_score() as if they were off CD?
        // NICE: take into account the CDs after the incoming action_seq

        if ! sonar_discharged {
            TORPEDO
        }
        else if ! torpedo_discharged {
            SONAR
        }
        // FIXME: this charges Sonar when position is known...
        else if them.is_position_narrow() { // NICE: && are we close enough?
            TORPEDO
        }
        else {
            SONAR
        }
    }

    fn should_sonar(&self, them: &Them, map: &Map) -> bool {
        if self.sonar_cooldown > 0 { return false }

        them.sonar_score(&map) > 0.5
    }

    fn parse_sonar_result(&self, result: &str) -> Option<TheirEvent> {
        if result != "NA" {
            let success = result == "Y";
            Some(TheirEvent::MySonar{ sector: self.last_sonar_sector, success })
        } else {
            None
        }
    }

    // TODO: play with combinations of Move + Torpedo
    fn should_fire(&self, them: &Them, map: &Map) -> Option<Coord> {
        if self.torpedo_cooldown > 0 { return None }
        // see Me::torpedo_score for an explanation on this magic number
        if !them.is_position_tracked() || them.ncandidates() > 12 { return None }

        let (target, score) = self.best_shot(them, map);
        if score > 29. {
            Some(target)
        } else {
            None
        }
    }

    fn best_shot(&self, them: &Them, map: &Map) -> (Coord, f32) {
        let targets = map.cells_within_distance(&self.pos, 4);
        let impacts = targets.iter().map(|target| them.torpedo_impact(target, &map));
        let mut scores = Vec::new();
        for (i, (naffected, damage_sum)) in impacts.enumerate() {
            let my_damage = self.torpedo_damage(&targets[i], map);
            let distance_from_me = map.distance(&self.pos, &targets[i]).unwrap();
            scores.push(Self::torpedo_score(
                them.ncandidates(), naffected, damage_sum, my_damage, distance_from_me
            ))
        }

        targets
            .into_iter()
            .zip(scores.into_iter())
            .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap())
            .unwrap()
    }

    fn torpedo_damage(&self, target: &Coord, map: &Map) -> usize {
        if *target == self.pos { 2 }
        else if map.area_of_effect(&target).contains(&self.pos) { 1 }
        else { 0 }
    }

    fn torpedo_score(
        ncandidates: usize, naffected: usize, damage_sum: usize,
        my_damage: usize, distance_from_me: usize
    ) -> f32 // should be usize instead?
    {
        let avg_candidates_dmg = damage_sum as f32 / ncandidates as f32;
        // max possible naffected = 9;
        let naffected_proportion =  naffected as f32 / 9.;
        let my_dmg = my_damage as f32;
        let dmg_difference = avg_candidates_dmg - my_dmg;

        let mut score = 0.;
        let mut weigh = |weight| score += weight;

        // should I be more careful with float comparisons?
        if avg_candidates_dmg <= my_dmg  { weigh(-100.) }
        // known_position && position == target, so avg_damage ~= 2.0
        else if avg_candidates_dmg > 1.95 { weigh(100.) }
        // NICE: revise this. I'm requiring (8+1*2)/12 affected, or 4/5
        //       perhaps ncandidates should be sqrted in that division...
        //       in the meantime, I'm changing to 12 the ncandidates cutoff in Me::should_torpedo.
        else if avg_candidates_dmg > 0.8 && dmg_difference > 0.3 {
            let avg_dmg_weight = 20. + 30.*((avg_candidates_dmg-0.8) / (2.0-0.8));
            let naffected_weight = 10. + 20.*(naffected_proportion);
            weigh(avg_dmg_weight + naffected_weight);
        }
        weigh(distance_from_me as f32 / 4.); // range: [0,1]
        // NICE: wait if I can narrow it down (Sonar available, their Silence on CD, etc.)

        score
    }
}

impl Them {
    fn new() -> Them {
        Them {
            lives: 0,
            pos_candidates: Vec::new(),
        }
    }

    fn ncandidates(&self) -> usize {
        self.pos_candidates.len()
    }

    fn is_position_known(&self) -> bool {
        self.ncandidates() == 1
    }

    #[allow(dead_code)]
    // cond: position is known
    fn position(&self) -> Coord {
        self.pos_candidates[0].clone()
    }

    fn is_position_tracked(&self) -> bool {
        self.ncandidates() != 0
    }

    // as in "narrowed down"
    fn is_position_narrow(&self) -> bool {
        1 < self.ncandidates()
            && self.ncandidates() <= 25
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
        }
        self.pos_candidates.sort_unstable();
        eprintln!("total:{} {:?}", self.ncandidates(), self.pos_candidates);
    }

    fn analyze_move(&mut self, dir: char, map: &Map) {
        if self.is_position_tracked() {
            self.pos_candidates =
                self.pos_candidates
                .iter()
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
                //TODO: trace_back (to discard visited)
            ).chain(iter::once(pos.clone()))
        }).collect();
        self.pos_candidates.sort_unstable();
        self.pos_candidates.dedup();
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

    // TODO: trace_back_candidate
    /*fn trace_back_candidate
    (
        &self, event_i: usize, candidate: &Coord, visited: &mut Vec<CoordSet>
    )
    {
        // need a termination condition on event_i. the latest Surface? origin?

        for i in 0..visited.len() {
            if ! self.analyze_backwards(event_i, &candidate, &mut visited[i]) {
                // if this one doesn't check out, the next few neither
                //  cause if visited.len() > 1, it comes from a Silence divergence
                //  and if the i_th position of a Silence is invalid, then you can't reach the later ones
                visited.truncate(i);
                break
            }
        }

        if ! visited.is_empty() {
            let candidate_before = self.before_event(event_i, &candidate); // Coord.before_move() goes here
            self.trace_back_candidate(event_i-1, candidate_before, visited)
        }

        // the final result of this recursion is visited itself (the presence or not of each element)
        // in a Silence origin, visited is like DIRECTIONS.each_do [{pos}, {pos, pos+1} .. {pos .. pos+4}]
        // for other origins (when position tracking begins?), visited.len = 1
    }*/

    fn sonar_score(&self, map: &Map) -> f32 {
        // NICE: bring Them.silence_cooldown into the fold
        if ! self.is_position_tracked() ||  self.is_position_known() {
            return 0.
        }
        let ncandidates = self.ncandidates();
        let min_discarded = self.sonar_discrimination(&map);
        let proportion = min_discarded as f32 / ncandidates as f32; // max = 0.5

        let answer =
            min_discarded != 0 && (
                ncandidates < 10
                || ncandidates < 25 && proportion > 0.2
                || proportion > 0.4
            );
        if answer { 1. } else { 0. }
    }

    // cond: position is tracked
    // ret = lower bound on pos_candidates to be discarded
    fn sonar_discrimination(&self, map: &Map) -> usize {
        let (_sector, most_candidates) = self.sector_most_candidates(&map);
        cmp::min(
            most_candidates,
            self.ncandidates() - most_candidates
        )
    }

    // cond: position is tracked
    // ret = (sector, number of candidates)
    fn sector_most_candidates(&self, map: &Map) -> (usize, usize) {
        let sectors =
            self.pos_candidates
            .iter()
            .map(|pos| map.sector_from(pos));

        let mut sector_counts = HashMap::new();
        for sector in sectors {
            let sector_count = sector_counts.entry(sector).or_insert(0);
            *sector_count += 1;
        }
        let max =
            sector_counts
            .iter()
            .max_by(|a, b| a.1.cmp(&b.1));

        match max {
            Some((&sector, &ncandidates)) => (sector, ncandidates),
            None => panic!("Them::sector_most_candidates: used without position_is_tracked?")
        }
    }

    // ret: (|affected candidates|, sum(damage))
    // sum(damage) cause they can lose 1-2 lives
    fn torpedo_impact(&self, target: &Coord, map: &Map) -> (usize, usize) {
        let area_of_effect = map.area_of_effect(&target);
        let damages: Vec<usize> =
            area_of_effect
            .iter()
            .filter_map(|pos| {
                if self.pos_candidates.contains(&pos) { Some(1) }
                else { None }
            })
            .collect();
        let mut damages_sum = damages.iter().sum();
        if self.pos_candidates.contains(&target) { damages_sum += 1 }

        (damages.len(), damages_sum)
    }
}

impl Me {
    fn next_actions(&self, global: &Global, them: &Them) -> ActionSeq {
        let mut action_seq = ActionSeq::new();
        let viable_moves = self.viable_moves(&global.map);
        let have_to_surface = viable_moves.is_empty();
        let should_sonar = self.should_sonar(&them, &global.map);

        if have_to_surface {
            action_seq.push(Action::Surface)
        }
        if let Some(target) = self.should_fire(&them, &global.map) {
            action_seq.push(Action::Torpedo{target});
        }
        if !have_to_surface {
            // TODO: better pathing, including Silence
            let device = self.device_to_charge(them);
            action_seq.push(
                viable_moves[0].but_charge(device)
            )
        }
        if should_sonar {
            let (sector, _) = them.sector_most_candidates(&global.map);
            action_seq.push(Action::Sonar { sector });
        }

        action_seq
    }
}
