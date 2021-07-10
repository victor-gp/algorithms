# frozen_string_literal: true

def with_input
  if ARGV.empty? # running in judge
    def byebug = nil # don't mind byebug calls
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

foo = with_input do |input|
  input.read
  input.parse
end
