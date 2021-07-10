# frozen_string_literal: true

# https://www.codingame.com/training/medium/cooperative-mate-with-rook

STDOUT.sync = true # DO NOT REMOVE
# Find the shortest sequence of cooperative moves to checkmate the black king.

# moving_player: Either black or white
# white_king: Position of the white king, e.g. a2
# white_rook: Position of the white rook
# black_king: Position of the black king
moving_player, white_king, white_rook, black_king = gets.split(" ")

# Write an action using puts
# To debug: STDERR.puts "Debug messages..."


# Write a sequence of moves (a single move is, e.g. a2b1) separated by spaces
puts "h5h1 a1a2 b3b8 h1f1"
