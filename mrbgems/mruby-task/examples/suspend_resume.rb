# Suspend and Resume Example
# Demonstrates manual task control

puts "=== Suspend/Resume Demo ==="
puts

$counter = 0

# Create a worker task
worker = Task.new(name: "worker") do
  10.times do |i|
    puts "Worker: iteration #{i}, counter=#{$counter}"
    $counter += 1
    sleep 0.2
  end
  puts "Worker: finished!"
end

# Create a controller task
controller = Task.new(name: "controller") do
  sleep 0.5

  puts "\nController: Suspending worker..."
  worker.suspend
  puts "Controller: Worker status = #{worker.status}"

  sleep 1

  puts "\nController: Incrementing counter while worker is suspended..."
  5.times do
    $counter += 10
    puts "Controller: counter=#{$counter}"
    sleep 0.2
  end

  puts "\nController: Resuming worker..."
  worker.resume
  puts "Controller: Worker status = #{worker.status}"

  # Wait for worker to finish
  worker.join
end

# Run the scheduler
Task.run

puts "\n=== Final counter value: #{$counter} ==="
