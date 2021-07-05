# https://www.codingame.com/training/hard/kakuro-solver

require 'forwardable'

class Kakuro
  def initialize(height, width, grid_s)
    @height = height
    @width = width
    @grid = self.class.parse_grid(grid_s)
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
    solve_inner!(height - 1, width)
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
  end

  # inv: all variables before (i,j) have been assigned (locally) correct values
  def solve_inner!(i, j)
    next_variable = next_variable(i, j)
    unless j == width # don't validate at depth 0
      return false unless validate_lines_between([i, j], next_variable)
    end
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

  # reverse order traversal, not including (i,j), nil if none left
  def next_variable(i, j)
    next_cell  = next_cell(i, j)
    return nil if next_cell.nil?

    # search what's left of the current row
    i, j = next_cell
    var_j = j.downto(0).find{ |j| grid[i][j].variable? }
    return i, var_j if var_j

    # search rows above current row
    (i - 1).downto(0).each do |i|
      var_j = (width - 1).downto(0).find{ |j| grid[i][j].variable? }
      return i, var_j if var_j
    end

    nil
  end

  # returns nil if (i,j) is the first cell
  def next_cell(i, j)
    j -= 1
    return [i,j] if j >= 0

    i -= 1
    j = width - 1

    i >= 0 ? [i, j] : nil
  end

  # cond: (i,j) is a variable cell
  def candidates_for(i, j)
    candidates = (1..9).to_a
    unique_constrain!(candidates, i, j)
    sums_constrain!(candidates, i, j)
    candidates
  end

  def unique_constrain!(candidates, i, j)
    (0...width).each{ |j| grid[i][j].unique_constrain!(candidates) }
    (0...height).each{ |i| grid[i][j].unique_constrain!(candidates) }
  end

  def sums_constrain!(candidates, i, j)
    nvars = 0
    reverse_row_accumulation(i) do |i, j, acc|
      nvars += 1 if grid[i][j].variable?
      grid[i][j].right_constrain!(candidates, acc, nvars)
      return candidates if candidates.empty?
    end

    nvars = 0
    reverse_column_accumulation(j) do |i, j, acc|
      nvars += 1 if grid[i][j].variable?
      grid[i][j].down_constrain!(candidates, acc, nvars)
      return candidates if candidates.empty?
    end
  end

  # cond: start is a valid cell, end may be nil (finished traversal)
  # ret: true if all rows/cols ending between start and end are valid
  def validate_lines_between(start, end_)
    start_i, start_j = start
    # if the traversal finished, make sure to check row 0 and col 0
    end_i, end_j = end_ ? end_ : [-1, -1]

    return false unless
      (start_i).downto(end_i + 1).all?{ |i| validate_row(i) }

    if end_i <= 0 # only check columns if traversal is on first row
      # check from last column if we started on another row
      start_j = width - 1 if start_i > 0
      # don't check the current column
      end_j += 1

      return false unless
        (start_j).downto(end_j).all?{ |j| validate_column(j) }
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

module UniqueConstraint
  def delete_value!(candidates)
    candidates.delete(value)
  end
end

module SumConstraint
  # cond: candidates is sorted
  def sum_constrain!(candidates, accumulated, nvars)
    nvars -= 1 # other vars, w/o the one being assigned
    min_others = candidates[...nvars].sum
    max_difference = value - accumulated - min_others
    offset = candidates.length - nvars
    max_others = candidates[offset..].sum
    min_difference = value - accumulated - max_others

    candidates.select{ |x| min_difference <= x && x <= max_difference }
  end
end

module NoConstraints
  def constrain_not_1(_candidates) = nil
  def constrain_not_3(_candidates, _accumulated, _nvars) = nil

  alias :unique_constrain! :constrain_not_1
  alias :right_constrain!  :constrain_not_3
  alias :down_constrain!   :constrain_not_3
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
  include UniqueConstraint
  include NoConstraints
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

  def unique_constrain!(candidates)
    value ? delete_value!(candidates) : nil
  end
end

module Fixed
  def variable? = false
end

module NotSummable
  def add_to(accumulated)
    accumulated
  end
end

class X
  include Fixed
  include NotSummable
  include NoConstraints
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
  include UniqueConstraint
  include NoConstraints
  include NoValidation

  def to_s
    value.to_s
  end

  def add_to(accumulated)
    accumulated + value
  end

  alias :unique_constrain! :delete_value!
end

class SingleSum
  include FixedSingleValue
  include NotSummable
  include SumConstraint
  include NoConstraints
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

  def unique_constrain!(_candidates) = nil

  def_delegators :@rightside_sum, :right_constrain!, :validate_right
  def_delegators :@downward_sum,  :down_constrain!,  :validate_down
end


height, width = gets.split.map(&:to_i)
grid_s = $stdin.read
kakuro = Kakuro.new(height, width, grid_s)
kakuro.solve!
puts kakuro
