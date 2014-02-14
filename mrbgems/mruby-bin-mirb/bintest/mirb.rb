require 'open3'

assert('mirb normal operations') do
  o, s = Open3.capture2('bin/mirb', :stdin_data => "a=1\nb=2\na+b\n")
  assert_true o.include?('=> 3')
  assert_true o.include?('=> 2')
end

assert('regression for #1563') do
  o, s = Open3.capture2('bin/mirb', :stdin_data => "a=1;b=2;c=3\nb\nc")
  assert_true o.include?('=> 3')
end

assert('regression for #1706') do
  o, s = Open3.capture2('bin/mirb', :stdin_data => %{
    if false
      a = 1
      b = 1
    end
    puts "a: \#{a.inspect}"
    puts "b: \#{b.inspect}"
  })
  assert_true o.include?('a: nil')
  assert_true o.include?('b: nil')

  o, s = Open3.capture2('bin/mirb', :stdin_data => %{
    a = 1
    b = 2
    if false
      c = 3
    end
    puts "c: \#{c.inspect}"
  })
  assert_true o.include?('c: nil')
end
