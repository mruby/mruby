# Task Inspection Example
# Demonstrates task status and inspect methods

puts "=== Task Inspection Demo ==="
puts

# Create some tasks
worker1 = Task.new(name: "worker-1") do
  puts "Worker 1: Starting"
  puts "Worker 1: My status is #{Task.current.status}"
  puts "Worker 1: My inspect is #{Task.current.inspect}"
  sleep 0.5
  puts "Worker 1: Finished"
end

worker2 = Task.new(name: "worker-2") do
  sleep 1
  puts "Worker 2: Finished"
end

# Inspect before running
puts "Initial task states:"
puts "  worker1: #{worker1.inspect}"
puts "           status=#{worker1.status}"
puts "  worker2: #{worker2.inspect}"
puts "           status=#{worker2.status}"
puts

# Create an observer task
observer = Task.new(name: "observer") do
  sleep 0.2

  puts "\nObserver checking task states:"
  puts "  worker1: #{worker1.inspect}"
  puts "           status=#{worker1.status}"
  puts "  worker2: #{worker2.inspect}"
  puts "           status=#{worker2.status}"

  sleep 0.5

  # Suspend worker2
  worker2.suspend
  puts "\nObserver suspended worker2:"
  puts "  worker2: #{worker2.inspect}"
  puts "           status=#{worker2.status}"

  sleep 0.3

  # Resume and terminate
  worker2.resume
  sleep 0.1
  worker2.terminate

  puts "\nObserver terminated worker2:"
  puts "  worker2: #{worker2.inspect}"
  puts "           status=#{worker2.status}"
end

# Run the scheduler
Task.run

puts "\n=== Final states ==="
puts "  worker1: #{worker1.inspect} status=#{worker1.status}"
puts "  worker2: #{worker2.inspect} status=#{worker2.status}"
puts "  observer: #{observer.inspect} status=#{observer.status}"
