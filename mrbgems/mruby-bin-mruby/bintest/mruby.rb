assert('regression for #1564') do
  o = `bin/mruby -e '<<' 2>&1`
  assert_equal o, "-e:1:2: syntax error, unexpected tLSHFT\n"
  o = `bin/mruby -e '<<-' 2>&1`
  assert_equal o, "-e:1:3: syntax error, unexpected tLSHFT\n"
end

assert('regression for #1572') do
  system "echo 'p \"ok\"' > /tmp/1572.rb"
  system "bin/mrbc -g -o /tmp/1572.mrb /tmp/1572.rb"
  o = `bin/mruby -b /tmp/1572.mrb`.strip
  assert_equal o, '"ok"'
end
