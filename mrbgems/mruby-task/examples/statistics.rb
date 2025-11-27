# Task Statistics Example
# Demonstrates Task.stat for monitoring scheduler state

def print_stats(label)
  stat = Task.stat
  puts "\n#{label}"
  puts "  Tick: #{stat[:tick]}, Next wakeup: #{stat[:wakeup_tick]}"
  puts "  Ready:     #{stat[:ready][:count]} tasks"
  puts "  Waiting:   #{stat[:waiting][:count]} tasks"
  puts "  Suspended: #{stat[:suspended][:count]} tasks"
  puts "  Dormant:   #{stat[:dormant][:count]} tasks"
end

puts "=== Task Statistics Demo ==="

print_stats("Initial state:")

# Create several tasks
workers = []
5.times do |i|
  workers << Task.new(name: "worker-#{i}") do
    sleep 0.5
    puts "Worker #{i} completed"
  end
end

print_stats("After creating 5 tasks:")

# Create a monitor task
monitor = Task.new(name: "monitor") do
  sleep 0.1
  print_stats("Monitor: After 0.1s:")

  # Suspend a couple tasks
  workers[2].suspend
  workers[3].suspend

  print_stats("Monitor: After suspending 2 tasks:")

  sleep 0.5

  print_stats("Monitor: After 0.5s more:")

  # Resume suspended tasks
  workers[2].resume
  workers[3].resume

  print_stats("Monitor: After resuming tasks:")

  # Wait for all workers
  workers.each(&:join)
end

# Run the scheduler
Task.run

print_stats("Final state:")

puts "\n=== Demo complete ==="
