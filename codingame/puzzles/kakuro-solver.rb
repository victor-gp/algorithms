# https://www.codingame.com/training/hard/kakuro-solver

require 'forwardable'

class Kakuro
  def initialize(height, width, grid_s)
    @height = height
    @width = width
    @grid = self.class.parse_grid(grid_s)
    # TODO: vars_left, vars_above
  end

  attr_reader :height, :width
  attr_accessor :grid

  def to_s
    grid.map do |row|
      row.map(&:to_s).join(',')
    end.join("\n")
  end

  # cond: the Kakuro has a solution
  def solve!
    solve_inner!(0, -1)
  end

  private

  class << self
    def parse_grid(grid_s)
      grid = []
      grid_s.lines.each do |line|
        p = '\|' # pipe regex
        np = "[^#{p}]" # no-pipe regex
        cells_s = line.scan(/#{p}(#{np}*)(?=#{p})/) # remove pipes
                      .map{ |match_ary| match_ary[0].strip } # trim whitespace
        cells = cells_s.map{ |cell_s| parse_cell(cell_s) }
        grid.push cells
      end

      grid
    end

    def parse_cell(cell_s)
      if cell_s == ''
        VariableDigit.new
      elsif cell_s == 'X'
        X.new
      elsif /^(?<sum>\d+)\\$/ =~ cell_s
        DownwardSum.new(sum.to_i)
      elsif /^\\(?<sum>\d+)$/ =~ cell_s
        RightsideSum.new(sum.to_i)
      elsif /^(?<sumL>\d+)\\(?<sumR>\d+)$/ =~ cell_s
        DoubleSum.new(
          DownwardSum.new(sumL.to_i),
          RightsideSum.new(sumR.to_i)
        )
      else
        FixedDigit.new(cell_s.to_i)
      end
    end

    def compute_var_counts(grid)
    end
  end

  # inv: all variables before (i,j) have been assigned (locally) correct values
  def solve_inner!(i, j)
    next_variable = next_variable(i, j)
    # FIXME: this seems to return prematurely at recursion depth 0,
    #        if the first line contains no variables and/or there's DoubleSum in the mix
    return false unless validate_lines_between([i, j], next_variable)
    return true if next_variable.nil?

    i, j = next_variable
    candidates = candidates_for(i, j)
    candidates.each do |candidate|
      grid[i][j].value = candidate
      return true if solve_inner!(i, j)
    end

    grid[i][j].value = nil
    false
  end

  # not including (i,j), nil if none left
  def next_variable(i, j)
    # TODO: invert traversal: bottom to top, right to left
    #       I can narrow candidates sooner like that (more constraints in the way)
    next_cell  = next_cell(i, j)
    return nil if next_cell.nil?

    # search what's left of the current row
    i, j = next_cell
    var_j = (j...width).find{ |j| grid[i][j].variable? }
    return i, var_j if var_j

    # search rows below current row
    (i+1...height).each do |i|
      var_j = (0...width).find{ |j| grid[i][j].variable? }
      return i, var_j if var_j
    end

    nil
  end

  # returns nil if (i,j) is the last cell
  def next_cell(i, j)
    j += 1
    unless j < width
      i += 1
      j = 0
    end

    i < height ? [i, j] : nil
  end

  # cond: (i,j) is a variable cell
  def candidates_for(i, j)
    candidates = (1..9).to_a
    reverse_row_accumulation(i) do |i, j, acc|
      grid[i][j].right_constrain!(candidates, acc)
      return candidates if candidates.empty?
    end
    reverse_column_accumulation(j) do |i, j, acc|
      grid[i][j].down_constrain!(candidates, acc)
      return candidates if candidates.empty?
    end

    candidates
  end

  # cond: start is a valid cell, end may be nil (finished traversal)
  # ret: true if all rows/cols ending between start and end are valid
  def validate_lines_between(start, end_)
    start_i, start_j = start
    start_j = 0 if start_j == -1
    end_i, end_j = end_ ? end_ : [height, width]

    return false unless (start_i...end_i).all?{ |i| validate_row(i) }

    if end_i >= height - 1
      start_j = 0 if start_i < height - 1

      return false unless (start_j...end_j).all?{ |j| validate_column(j) }
    end

    true
  end

  def validate_row(i)
    reverse_row_accumulation(i) do |i, j, acc|
      return false unless grid[i][j].validate_right(acc)
    end
    true
  end

  def validate_column(j)
    reverse_column_accumulation(j) do |i, j, acc|
      return false unless grid[i][j].validate_down(acc)
    end
    true
  end

  def reverse_row_accumulation(i)
    acc = 0
    (width - 1).downto(0).each do |j|
      acc = grid[i][j].add_to(acc)
      yield i, j, acc
    end
  end

  def reverse_column_accumulation(j)
    acc = 0
    (height - 1).downto(0).each do |i|
      acc = grid[i][j].add_to(acc)
      yield i, j, acc
    end
  end
end

module NotSummable
  def add_to(accumulated)
    accumulated
  end
end

module ValueConstraint
  def value_constrain!(candidates, _accumulated)
    candidates.delete(value)
  end
end

module SumConstraint
  def sum_constrain!(candidates, accumulated)
    # TODO: find a lower bound too, with unassigned_vars and assuming candidates is sorted
    difference = value - accumulated
    candidates.select{ |x| x <= difference }
  end
end

module NoConstraint
  def constrain_not(_candidates, _accumulated) = nil

  alias :right_constrain! :constrain_not
  alias :down_constrain!  :constrain_not
end

module Validation
  def validate(accumulated)
    value == accumulated
  end
end

module NoValidation
  def validate_not(accumulated)
    true
  end

  alias :validate_right :validate_not
  alias :validate_down  :validate_not
end

class VariableDigit
  include ValueConstraint
  include NoValidation

  def initialize
    @value = nil
  end

  attr_accessor :value

  def to_s
    value ? value.to_s : '?'
  end

  def variable? = true

  def add_to(accumulated)
    value ? accumulated + value : accumulated
  end

  def right_constrain!(candidates, _accumulated)
    value_constrain!(candidates, _accumulated) if value
  end

  alias :down_constrain! :right_constrain!
end

module Fixed
  def variable? = false
end

class X
  include Fixed
  include NotSummable
  include NoConstraint
  include NoValidation

  def to_s = 'X'
end

module FixedSingleValue
  include Fixed

  def initialize(value)
    @value = value
  end

  attr_reader :value
end

class FixedDigit
  include FixedSingleValue
  include ValueConstraint
  include NoValidation

  def to_s
    value.to_s
  end

  def add_to(accumulated)
    accumulated + value
  end

  alias :right_constrain! :value_constrain!
  alias :down_constrain!  :value_constrain!
end

class SingleSum
  include FixedSingleValue
  include NotSummable
  include SumConstraint
  include NoConstraint
  include Validation
  include NoValidation
end

class RightsideSum < SingleSum
  def to_s
    "\\#{value}"
  end

  alias :right_constrain! :sum_constrain!
  alias :validate_right   :validate
end

class DownwardSum < SingleSum
  def to_s
    "#{value}\\"
  end

  alias :down_constrain! :sum_constrain!
  alias :validate_down   :validate
end

class DoubleSum
  include Fixed
  include NotSummable
  extend  Forwardable

  def initialize(downward_sum, rightside_sum)
    @rightside_sum = rightside_sum
    @downward_sum = downward_sum
  end

  attr_reader :rightside_sum, :downward_sum

  def to_s
    "#{downward_sum.value}\\#{rightside_sum.value}"
  end

  def_delegators :@rightside_sum, :right_constrain!, :validate_right
  def_delegators :@downward_sum,  :down_constrain!,  :validate_down
end


height, width = gets.split.map(&:to_i)
grid_s = $stdin.read
kakuro = Kakuro.new(height, width, grid_s)
kakuro.solve!
puts kakuro
