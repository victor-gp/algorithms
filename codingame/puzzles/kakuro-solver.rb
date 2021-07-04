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

    nil
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
        RightsideSum.new(sum.to_i)
      elsif /^\\(?<sum>\d+)$/ =~ cell_s
        DownwardSum.new(sum.to_i)
      elsif /^(?<sumL>\d+)\\(?<sumR>\d+)$/ =~ cell_s
        CrossSums.new(
          RightsideSum.new(sumL.to_i),
          DownwardSum.new(sumR.to_i)
        )
      else
        FixedDigit.new(cell_s.to_i)
      end
    end
  end

  # inv: all variables before (i,j) have been assigned (locally) correct values
  def inner_solve!(i, j)
    next_variable = next_variable(i, j)
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
end

class VariableDigit
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

  def to_s
    "#{value}\\"
  end

  def right_constrain!(candidates, sum_so_far)
    difference = value - sum_so_far
    candidates.select{ |x| x <= difference }
    sum_so_far
  end

  def down_constrain!(candidates, sum_so_far) = sum_so_far
end

class DownwardSum
  include FixedSingleValue

  def to_s
    "\\#{value}"
  end

  def right_constrain!(candidates, sum_so_far) = sum_so_far

  def down_constrain!(candidates, sum_so_far)
    difference = value - sum_so_far
    candidates.select{ |x| x <= difference }
    sum_so_far
  end
end

class CrossSums
  def initialize(rightside_sum, downward_sum)
    @rightside_sum = rightside_sum
    @downward_sum = downward_sum
  end

  attr_reader :rightside_sum, :downward_sum

  def to_s
    "#{rightside_sum.value}\\#{downward_sum.value}"
  end

  def right_constrain!(candidates, sum_so_far)
    rightside_sum.right_constrain!(candidates, sum_so_far)
  end

  def down_constrain!(candidates, sum_so_far)
    downward_sum.down_constrain!(candidates, sum_so_far)
  end
end


height, width = gets.split.map(&:to_i)
grid_s = $stdin.read
kakuro = Kakuro.new(height, width, grid_s)
kakuro.solve!
puts kakuro
