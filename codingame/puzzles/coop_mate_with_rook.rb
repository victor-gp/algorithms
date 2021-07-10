# frozen_string_literal: true

# https://www.codingame.com/training/medium/cooperative-mate-with-rook

require 'forwardable'

class Configuration
  def initialize(whites_move, white_king, white_rook, black_king)
    @whites_move = whites_move
    @white_king = white_king
    @white_rook = white_rook
    @black_king = black_king

    @whites = [white_king, white_rook]
    @blacks = [black_king]
    @allies_hash = precompute_allies(whites, blacks)
    @others_hash = precompute_others(whites + blacks)

    @visited_mem = {} # to avoid loops in a given sequence of moves (whites only)
    @checkmate_length_mem = {} # to return early on configurations that can't improve the current best result

    @nconfs = 0
  end

  attr_reader :white_king, :white_rook, :black_king, :nconfs

  def shortest_checkmate
    shorter_than = 13 # constraint defined in the statement
    shortest_checkmate_inner(shorter_than).reverse
  end

  def allies(piece)
    allies_hash[piece]
  end

  def adversaries(piece)
    white?(piece) ? blacks : whites
  end

  def others(piece)
    others_hash[piece]
  end

  private

  attr_accessor :whites_move
  attr_writer :nconfs
  attr_reader :whites, :blacks, :allies_hash, :others_hash,
              :visited_mem, :checkmate_length_mem

  def precompute_allies(whites, blacks)
    allies = {}
    whites.each do |piece|
      allies[piece] = whites.difference([piece])
    end
    blacks.each do |piece|
      allies[piece] = blacks.difference([piece])
    end
    allies
  end

  def precompute_others(pieces)
    others = {}
    pieces.each do |piece|
      others[piece] = pieces.difference([piece])
    end
    others
  end

  # returns the shortest checkmate from self, in reverse order
  def shortest_checkmate_inner(shorter_than)
    return if shorter_than.zero?

    checkmate_length = memoized_checkmate_length
    if checkmate_length
      return unless checkmate_length < shorter_than

      shorter_than = checkmate_length
    end

    # todo: compare shorter_than with distances (king to king?)

    self.nconfs = nconfs + 1
    moves = heuristic_moves
    return [] if moves.empty? && !whites_move # checkmate

    # prepare iteration on moves
    shortest_checkmate = nil
    store_visited if whites_move
    self.whites_move = !whites_move
    shorter_than -= 1

    moves.each do |move|
      origin = move.apply
      checkmate = shortest_checkmate_inner(shorter_than)
      if checkmate
        shorter_than = checkmate.length
        # push should be cheaper than unshift, we can reverse outside
        shortest_checkmate = (checkmate.push move)
      end
      move.piece.move!(origin)
    end

    # restore previous state
    self.whites_move = !whites_move
    restore_visited if whites_move
    store_checkmate_length(shortest_checkmate.length) if shortest_checkmate

    shortest_checkmate
  end

  def white?(piece)
    piece != black_king
  end

  def heuristic_moves
    # todo: rewrite this into 1-2 traversals + sort and a single array?
    if whites_move
      whites
        .map { |pc| pc.viable_moves(self) }
        .flatten!
        .map { |mv| [mv, white_moves_heuristic(mv)] }
        .select { |_, he| he < 200 }
        .sort_by! { |_, he| he }
        .map { |mv, _| mv }
    else
      moves = black_king.viable_moves(self)
      moves_heuristics = moves.map { |mv| [mv, black_moves_heuristic(mv)] }
      moves_heuristics.select! { |_, he| he < 1000 }
      moves_heuristics.sort_by! { |_, he| he }
      moves_heuristics.map { |mv, _| mv }
    end
  end

  def white_moves_heuristic(move)
    score = 0
    score += 1000 if to_visited?(move)
    score += 1000 if move.away_from?(black_king) # make it closer, not closer or equal
    origin = move.apply
    score += 100 unless black_king.checkmate?(self)
    move.piece.move!(origin)
    score += 3 * move.distance_moved # incentivize King moves
    score += black_king.distance(move.dest)
    score
  end

  def black_moves_heuristic(move)
    score = 0
    score += 100 unless move.dest.next_to_border?
    # should not move away from border, but should be able to (exhaust all possibilities)
    #todo: distance to border
    score += white_king.distance(move.dest)
    score
  end

  def store_visited
    visited_mem[serialize_whites] = true
  end

  def restore_visited
    visited_mem[serialize_whites] = false
  end

  def to_visited?(move)
    origin = move.apply
    serialized = serialize_whites
    move.piece.move!(origin)
    visited_mem[serialized]
  end

  def serialize_whites
    #"#{white_king.pos}#{white_rook.pos}"
    white_king.serialize + 64 * white_rook.serialize
  end

  def store_checkmate_length(nmoves)
    checkmate_length_mem[serialize_conf] = nmoves
  end

  def memoized_checkmate_length
    checkmate_length_mem[serialize_conf]
  end

  def serialize_conf
    white_king.serialize + 64 * (
      white_rook.serialize + 64 * (
        black_king.serialize + 64 * (
          whites_move ? 0 : 1
        )
      )
    )
  end

  # https://www.chess.com/article/view/basic-checkmates-king-and-rook-mate
  def box_size; end
end

class Piece
  extend Forwardable

  def initialize(position)
    @pos = position
  end

  attr_reader :pos

  def_delegators :pos, :move!, :in_the_way?, :serialize

  def viable_moves(conf)
    moves = allowed_moves(conf)
    adversaries = conf.adversaries(self)
    moves.select! do |mv|
      adversaries.none? { |adv| adv.within_a_move?(mv.dest, conf) }
    end
    moves
  end

  def allowed_moves(conf)
    dests = reachable_positions
    others = conf.others(self)
    dests.select! do |dest|
      others.none? { |pc| pc.in_the_way?(pos, dest) }
    end
    dests.map! { |dest| Move.new(self, dest) }
  end

  def within_a_move?(pos, conf)
    moves = allowed_moves(conf)
    moves.any? { |mv| mv.dest == pos }
  end
end

class King < Piece
  def reachable_positions
    pos.adjacents
  end

  def distance(some_pos)
    # pos.diagonal_distance(some_pos)
    pos.straight_distance(some_pos)
    # diagonal distance (to BK) doesn't help the heuristic
    #todo: I should compare the number of BK' moves it threatens (and score it higher)
  end

  def box(conf)
    pos.box(
      conf.adversaries(self).map(&:pos)
    )
  end
end

class BlackKing < King
  def checkmate?(conf)
    viable_moves(conf).empty?
  end
end

class Rook < Piece
  def reachable_positions
    pos.horizontals + pos.verticals
  end

  def distance(some_pos)
    pos.straight_distance(some_pos)
  end

  # fixme: viable moves include those where they have coverage by the king!
  def viable_moves(conf)
    moves = super(conf)
    moves.select! do |mv|
      # only allow moves towards the last 2 cols/rows
      dest = mv.dest
      changing_component = dest.col == pos.col ? :row : :col
      changed_component = dest.send(changing_component)
      Coord.distance_to_border(changed_component) <= 2

      # todo: only allow checkmates
    end
    moves
  end
end

class Coord
  class << self
    def parse(coord_s)
      col = coord_s.chars[0].ord - 'a'.ord
      row = coord_s.chars[1].to_i - 1 # align to 0
      Coord.new(col, row)
    end
  end

  attr_reader :col, :row

  def to_s
    letter_col = (col + 'a'.ord).chr
    "#{letter_col}#{row + 1}"
  end

  def ==(other)
    col == other.col && row == other.row
  end

  def move!(coord)
    @col = coord.col
    @row = coord.row
  end

  def adjacents
    # todo: rewrite these as ternary ops
    cmin = [col - 1, 0].max
    cmax = [col + 1, 7].min
    rmin = [row - 1, 0].max
    rmax = [row + 1, 7].min
    coord_pairs = (cmin..cmax).to_a.product (rmin..rmax).to_a
    coord_pairs.delete([col, row])
    coord_pairs.map { |c, r| Coord.new(c, r) }
  end

  def horizontals
    cols = (0...col).to_a + (col + 1..7).to_a
    cols.map { |c| Coord.new(c, row) }
  end

  def verticals
    rows = (0...row).to_a + (row + 1..7).to_a
    rows.map { |r| Coord.new(col, r) }
  end

  def in_the_way?(origin, dest)
    if origin.row == dest.row # horizontal
      min, max = [origin.col, dest.col].sort
      col == min && min < col && col <= max
    elsif origin.col == dest.col # vertical
      min, max = [origin.row, dest.row].sort
      row == min && min < row && row <= max
    else # diagonal
      cmin, cmax = [origin.col, dest.col].sort
      col - origin.col == row - origin.row &&
        (cmin..cmax).cover?(col)
    end
  end

  def straight_distance(other)
    (col - other.col).abs + (row - other.row).abs
  end

  def diagonal_distance(other)
    [(col - other.col).abs, (row - other.row).abs].max
  end

  def next_to_border?
    Coord.distance_to_border(col) == 1 ||
      self.class.distance_to_border(row) == 1
  end

  class << self
    def distance_to_border(component)
      (component < 4 ? component : 7 - component) + 1
    end
  end

  def serialize
    8 * col + row
  end

  private

  def initialize(col, row)
    @col = col
    @row = row
  end
end

class Move
  def initialize(piece, dest)
    @piece = piece
    @dest = dest
  end

  attr_reader :piece, :dest

  # side effect: applies the move
  def to_s
    "#{piece.pos}#{dest}"
  end

  def str_and_apply
    str = to_s
    apply
    str
  end

  def apply
    origin = piece.pos.clone
    piece.move!(dest)
    origin
  end

  #todo: to_threatened?(adversaries), to_covered?(allies)

  def approach?(other_piece)
    delta_origin = piece.distance(other_piece.pos)
    origin = apply
    delta_dest = piece.distance(other_piece.pos)
    piece.move!(origin)

    delta_dest < delta_origin
  end

  def distance_moved
    piece.distance(dest)
  end

  def away_from?(other_piece)
    before = piece.distance(other_piece.pos)
    origin = apply
    after = piece.distance(other_piece.pos)
    piece.move!(origin)

    before < after
  end
end

def with_input
  if ARGV.empty? # running in judge
    def byebug = nil # allows submitting to judge with byebug calls
    yield $stdin
  else # running locally
    require 'byebug'
    yield test_file(ARGV[0])
  end
end

def test_file(test_number)
  test_basename = File.basename(__FILE__).sub('.rb', test_number.to_s)
  test_relative_path = "test/#{test_basename}"
  raise "Test file not found: #{test_relative_path}" unless File.file?(test_relative_path)

  File.new(test_relative_path)
end

$stdout.sync = true # DO NOT REMOVE

conf = with_input do |input|
  tokens = input.gets.split
  whites_move = tokens[0] == 'white'
  white_king = King.new( Coord.parse(tokens[1]) )
  white_rook = Rook.new( Coord.parse(tokens[2]) )
  black_king = BlackKing.new( Coord.parse(tokens[3]) )

  Configuration.new(whites_move, white_king, white_rook, black_king)
end

moves = conf.shortest_checkmate
$stderr.puts "configurations tested: #{conf.nconfs}"
puts moves.map(&:str_and_apply).join(' ')
