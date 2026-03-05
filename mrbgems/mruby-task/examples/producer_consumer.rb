# Producer/Consumer Example
# Demonstrates task coordination using shared state

puts "=== Producer/Consumer Demo ==="
puts

# Shared buffer
$buffer = []
$max_items = 10
$produced = 0

# Producer task
producer = Task.new(name: "producer") do
  $max_items.times do |i|
    # Produce an item
    item = "item-#{i}"
    $buffer << item
    $produced += 1
    puts "Producer: created #{item} (buffer size: #{$buffer.size})"

    sleep 0.3
  end
  puts "Producer: finished producing #{$max_items} items"
end

# Consumer task
consumer = Task.new(name: "consumer") do
  consumed = 0

  while consumed < $max_items
    if $buffer.empty?
      puts "Consumer: buffer empty, waiting..."
      sleep 0.2
    else
      item = $buffer.shift
      consumed += 1
      puts "Consumer: consumed #{item} (#{consumed}/#{$max_items})"
      sleep 0.5
    end
  end

  puts "Consumer: finished consuming #{consumed} items"
end

# Monitor task
monitor = Task.new(name: "monitor") do
  loop do
    sleep 1
    puts "Monitor: produced=#{$produced}, buffer=#{$buffer.size}, remaining=#{$max_items - $produced}"
    break if $produced >= $max_items && $buffer.empty?
  end
end

# Run the scheduler
Task.run

puts "\n=== All tasks completed ==="
puts "Final state: produced=#{$produced}, buffer=#{$buffer.size}"
