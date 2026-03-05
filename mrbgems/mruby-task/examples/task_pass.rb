$global = 0
task = Task.new do
  10.times do |i|
    puts "Task #{i} is running"
    $global += 1
    sleep 1
  end
end

while true
  sleep 0.5
  if $global < 6
    puts "Global variable is less than 6, waiting..."
    Task.pass
  else
    break
  end
end

puts "Global variable reached 6, exiting loop"
Task.run
