# Priority Scheduling Example
# Lower priority values = higher priority (0 is highest)

puts "=== Priority Scheduling Demo ==="
puts

# Create tasks with different priorities
low_priority = Task.new(name: "low-priority", priority: 200) do
  5.times do |i|
    puts "  [Low Priority] iteration #{i}"
    sleep 0.1
  end
end

high_priority = Task.new(name: "high-priority", priority: 50) do
  5.times do |i|
    puts "[High Priority] iteration #{i}"
    sleep 0.1
  end
end

medium_priority = Task.new(name: "medium-priority", priority: 128) do
  5.times do |i|
    puts "    [Medium Priority] iteration #{i}"
    sleep 0.1
  end
end

puts "Created 3 tasks with different priorities:"
puts "  High:   priority=50"
puts "  Medium: priority=128"
puts "  Low:    priority=200"
puts
puts "Tasks will run in priority order (highest first)"
puts

# Run the scheduler
Task.run

puts
puts "=== All tasks completed ==="
