# Task::Queue Example
# Demonstrates producer/consumer coordination using Task::Queue.
# No polling required - consumers block until items are available.

puts "=== Task::Queue Producer/Consumer Demo ==="
puts

TOTAL_ITEMS = 10
q = Task::Queue.new
produced = 0
consumed = 0

producer = Task.new(name: "producer") do
  TOTAL_ITEMS.times do |i|
    item = "item-#{i}"
    q.push(item)
    produced += 1
    puts "Producer: pushed #{item}"
    sleep 0.1
  end
  q.close
  puts "Producer: closed queue after #{produced} items"
end

consumer1 = Task.new(name: "consumer-1") do
  loop do
    item = q.pop  # blocks until an item is available or queue closes
    break if item.nil?
    consumed += 1
    puts "Consumer-1: got #{item}"
    sleep 0.15
  end
  puts "Consumer-1: done"
end

consumer2 = Task.new(name: "consumer-2") do
  loop do
    item = q.pop
    break if item.nil?
    consumed += 1
    puts "Consumer-2: got #{item}"
    sleep 0.2
  end
  puts "Consumer-2: done"
end

Task.run

puts
puts "=== Summary ==="
puts "Produced: #{produced}"
puts "Consumed: #{consumed}"
puts "Queue size: #{q.size}"
puts "Queue closed: #{q.closed?}"
