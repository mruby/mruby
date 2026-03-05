# Simple Task Example
# Basic usage of mruby-task

puts "=== Simple Task Demo ==="
puts

# Create a simple task
Task.new(name: "hello") do
  3.times do |i|
    puts "Hello from task, iteration #{i}"
    sleep 0.5
  end
end

# Create another task
Task.new(name: "world") do
  3.times do |i|
    puts "World from task, iteration #{i}"
    sleep 0.7
  end
end

# Run the scheduler - blocks until all tasks complete
puts "Starting tasks..."
Task.run

puts "All tasks completed!"
