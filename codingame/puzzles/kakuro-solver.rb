# https://www.codingame.com/training/hard/kakuro-solver

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

  def solve!
    inner_solve!(0, -1)
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
        CrossSums.new(
          DownwardSum.new(sumL.to_i),
          RightsideSum.new(sumR.to_i)
        )
      else
        FixedDigit.new(cell_s.to_i)
      end
    end
  end

  # inv: all variables before (i,j) have been assigned (locally) correct values
  def inner_solve!(i, j)
    next_variable = next_variable(i, j)
    return false unless validate_lines_between([i, j], next_variable)
    return true if next_variable.nil?

    i, j = next_variable
    candidates = candidates_for(i, j)
    candidates.each do |candidate|
      grid[i][j].value = candidate
      return true if inner_solve!(i, j)
    end

    grid[i][j].value = nil
    false
  end

  # not including (i,j), nil if none
  def next_variable(i, j)
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

    acc = 0
    (width - 1).downto(0).each do |j|
      acc = grid[i][j].right_constrain!(candidates, acc)
    end

    acc = 0
    (height - 1).downto(0).each do |i|
      acc = grid[i][j].down_constrain!(candidates, acc)
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
    acc = 0
    (width - 1).downto(0).each do |j|
      acc = grid[i][j].add_to(acc)
      return false unless grid[i][j].validate_right(acc)
    end

    true
  end

  def validate_column(j)
    acc = 0
    (height - 1).downto(0).each do |i|
      acc = grid[i][j].add_to(acc)
      return false unless grid[i][j].validate_down(acc)
    end

    true
  end
end

module Summable
  def add_to(sum_so_far)
    sum_so_far + value
  end
end

module NotSummable
  def add_to(sum_so_far)
    sum_so_far
  end
end

module Validation
  def validate(sum_so_far)
    value == sum_so_far
  end
end

module NoValidation
  def validate_not(sum_so_far)
    true
  end

  alias :validate_right :validate_not
  alias :validate_down  :validate_not
end

class VariableDigit
  include Summable
  include NoValidation

  def initialize
    @value = nil
  end

  attr_accessor :value

  def to_s
    if value
      value.to_s
    else
      '?'
    end
  end

  def variable? = true

  # constrains candidates, returns updated sum_so_far
  def right_constrain!(candidates, sum_so_far)
    if value
      candidates.delete(value)
      sum_so_far += value
    end

    sum_so_far
  end

  alias :down_constrain! :right_constrain!
end

module Fixed
  def variable? = false
end

class X
  include Fixed
  include NotSummable
  include NoValidation

  def to_s = 'X'

  def right_constrain!(candidates, sum_so_far) = sum_so_far
  alias :down_constrain! :right_constrain!
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
  include Summable
  include NoValidation

  def to_s
    value.to_s
  end

  def right_constrain!(candidates, sum_so_far)
    candidates.delete(value)
    sum_so_far + value
  end

  alias :down_constrain! :right_constrain!
end

class RightsideSum
  include FixedSingleValue
  include NotSummable
  include Validation
  include NoValidation

  def to_s
    "\\#{value}"
  end

  def right_constrain!(candidates, sum_so_far)
    difference = value - sum_so_far
    candidates.select{ |x| x <= difference }
    sum_so_far
  end

  def down_constrain!(candidates, sum_so_far) = sum_so_far

  alias :validate_right :validate
end

class DownwardSum
  include FixedSingleValue
  include NotSummable
  include Validation
  include NoValidation

  def to_s
    "#{value}\\"
  end

  def right_constrain!(candidates, sum_so_far) = sum_so_far

  def down_constrain!(candidates, sum_so_far)
    difference = value - sum_so_far
    candidates.select{ |x| x <= difference }
    sum_so_far
  end

  alias :validate_down :validate
end

class CrossSums
  include Fixed

  def initialize(downward_sum, rightside_sum)
    @rightside_sum = rightside_sum
    @downward_sum = downward_sum
  end

  attr_reader :rightside_sum, :downward_sum

  def to_s
    "#{downward_sum.value}\\#{rightside_sum.value}"
  end

  def right_constrain!(candidates, sum_so_far)
    rightside_sum.right_constrain!(candidates, sum_so_far)
  end

  def down_constrain!(candidates, sum_so_far)
    downward_sum.down_constrain!(candidates, sum_so_far)
  end

  def validate_right(sum_so_far)
    rightside_sum.validate_right(sum_so_far)
  end

  def validate_down(sum_so_far)
    downward_sum.validate_down(sum_so_far)
  end
end


height, width = gets.split.map(&:to_i)
grid_s = $stdin.read
kakuro = Kakuro.new(height, width, grid_s)
kakuro.solve!
puts kakuro
