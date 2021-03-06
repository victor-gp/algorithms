# frozen_string_literal: true

# https://www.codingame.com/training/medium/connect-the-hyper-dots

class Point
  def initialize(label, coordinates)
    @label = label
    @coordinates = coordinates
  end

  attr_reader :label, :coordinates

  def self.parse(point_line)
    point_tokens = point_line.split
    label = point_tokens[0]
    coordinates = point_tokens[1..].map(&:to_i)

    Point.new(label, coordinates)
  end

  def self.zero(dimensions)
    coordinates = Array.new(dimensions, 0)
    Point.new(nil, coordinates)
  end

  def nearest_index(points)
    distances = points.map { |p| distance(p) }
    _, index = distances.each_with_index.min
    index
  end

  def distance(other_point)
    coord_pairs = coordinates.zip other_point.coordinates
    squares = coord_pairs.map { |a, b| (a - b)**2 }
    Math.sqrt(squares.sum)
  end

  def same_orthant?(other_point)
    coord_pairs = coordinates.zip other_point.coordinates
    coord_pairs.none? { |a, b| a * b < 0 }
  end
end

def read_input
  count, = gets.split.map(&:to_i)
  points = []
  count.times do
    point_line = gets
    points.push Point.parse(point_line)
  end

  points
end

def first_point_index(points)
  dimensions = points[0].coordinates.length
  origin = Point.zero(dimensions)
  origin.nearest_index(points)
end

def greedy_path!(points, current_index)
  return if points.length == 1

  current = points[current_index]
  points.delete_at(current_index)
  next_index = current.nearest_index(points)

  greedy_path!(points, next_index)
  points.insert(0, current)
end

def print_path(points)
  print points[0].label
  for point, previous in points[1..].zip points
    print ' ' unless point.same_orthant?(previous)
    print point.label
  end
  puts
end

points = read_input
first = first_point_index(points)
greedy_path!(points, first)
print_path(points)
