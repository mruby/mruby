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

assert('mirb -d option') do
  o, _ = Open3.capture2('bin/mirb', :stdin_data => "p $DEBUG\n")
  assert_true o.include?('=> false')
  o, _ = Open3.capture2('bin/mirb -d', :stdin_data => "p $DEBUG\n")
  assert_true o.include?('=> true')
end
