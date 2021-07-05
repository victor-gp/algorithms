# frozen_string_literal: true
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

  # inv: all variables past (i,j) have been assigned (locally) correct values
  def solve_inner!(i, j)
    next_variable = next_variable(i, j)
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

  # reverse order traversal, not including (i,j), returns nil if none left
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
    horizontal_unused = horizontal_unused(i, j)
    vertical_unused = vertical_unused(i, j)
    candidates = horizontal_unused.clone.intersection(vertical_unused)
    check_left_sums!(candidates, i, j, horizontal_unused)
    check_above_sums!(candidates, i, j, vertical_unused)

    candidates
  end

  def horizontal_unused(i, j)
    digits = (1..9).to_a

    (j+1...width).each do |j|
      break if grid[i][j].constrains_right?
      grid[i][j].unique_constrain!(digits)
      break if digits.empty?
    end

    (j-1).downto(0).each do |j|
      break if grid[i][j].constrains_right?
      grid[i][j].unique_constrain!(digits)
      break if digits.empty?
    end

    digits
  end

  def vertical_unused(i, j)
    digits = (1..9).to_a

    (i+1...height).each do |i|
      break if grid[i][j].constrains_down?
      grid[i][j].unique_constrain!(digits)
      break if digits.empty?
    end

    (i-1).downto(0).each do |i|
      break if grid[i][j].constrains_down?
      grid[i][j].unique_constrain!(digits)
      break if digits.empty?
    end

    digits
  end

  def check_left_sums!(candidates, i, j, horizontal_unused)
    is = i..i
    right_js = (j + 1)...width
    _, _, right_acc, _ = accumulate_until_constraint(is, right_js, :constrains_right?)

    left_js = (j - 1).downto(0)
    _, sum_j, left_acc, nvars = accumulate_until_constraint(is, left_js, :constrains_right?)

    return if sum_j.nil?

    grid[i][sum_j].right_constrain!(
      candidates, left_acc + right_acc, nvars, horizontal_unused
    )
  end

  def check_above_sums!(candidates, i, j, vertical_unused)
    js = j..j
    below_is = (i + 1)...height
    _, _, below_acc, _ = accumulate_until_constraint(below_is, js, :constrains_down?)

    above_is = (i - 1).downto(0)
    sum_i, _, above_acc, nvars = accumulate_until_constraint(above_is, js, :constrains_down?)

    return if sum_i.nil?

    grid[sum_i][j].down_constrain!(
      candidates, above_acc + below_acc, nvars, vertical_unused
    )
  end

  # cond: one parameter or the other has a single element
  def accumulate_until_constraint(i_range, j_range, end_marker)
    acc = 0
    nvars = 0
    i_range.each do |i|
      j_range.each do |j|
        return [i, j, acc, nvars] if
          grid[i][j].send(end_marker)
        acc = grid[i][j].add_to(acc)
        nvars += 1 if grid[i][j].variable?
      end
    end

    return [nil, nil, acc, nvars]
  end
end

module UniqueConstraint
  def delete_value!(candidates)
    candidates.delete(value)
  end
end

module SumConstraint
  # cond: unused_digits is sorted asc
  def sum_constrain!(candidates, accumulated, nvars, unused_digits)
    min_others = unused_digits[...nvars].sum
    max_difference = value - accumulated - min_others
    offset = unused_digits.length - nvars
    max_others = unused_digits.drop(offset).sum
    min_difference = value - accumulated - max_others

    candidates.select!{ |x| min_difference <= x && x <= max_difference }
  end
end

module NoConstraints
  def constrains_right? = false
  def constrains_down? = false

  def unique_constrain!(_) = nil
end

class VariableDigit
  include UniqueConstraint
  include NoConstraints

  def initialize
    @value = nil
  end

  attr_accessor :value

  def to_s
    value ? value.to_s : '?'
  end

  def variable? = value.nil?

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
end

class RightsideSum < SingleSum
  def to_s
    "\\#{value}"
  end

  def constrains_right? = true

  alias :right_constrain! :sum_constrain!
end

class DownwardSum < SingleSum
  def to_s
    "#{value}\\"
  end

  def constrains_down? = true

  alias :down_constrain! :sum_constrain!
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

  def_delegators :@rightside_sum, :right_constrain!, :constrains_right?
  def_delegators :@downward_sum,  :down_constrain!,  :constrains_down?
end

def with_input
  if File.file?('my-test.in')
    require 'byebug'
    yield File.open('my-test.in')
  else
    def byebug = nil
    yield $stdin
  end
end

kakuro = with_input do |input|
  height, width = input.gets.split.map(&:to_i)
  grid_s = input.read

  Kakuro.new(height, width, grid_s)
end
kakuro.solve!
puts kakuro
