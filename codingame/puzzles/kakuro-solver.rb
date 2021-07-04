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
end

class X
  def to_s
    'X'
  end
end

module FixedSingleValue
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
end

class RightsideSum
  include FixedSingleValue

  def to_s
    "#{value}\\"
  end
end

class DownwardSum
  include FixedSingleValue

  def to_s
    "\\#{value}"
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
end


height, width = gets.split.map(&:to_i)
grid_s = $stdin.read
kakuro = Kakuro.new(height, width, grid_s)
puts kakuro
