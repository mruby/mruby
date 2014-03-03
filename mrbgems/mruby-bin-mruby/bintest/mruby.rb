require 'tempfile'

assert('regression for #1564') do
  o = `bin/mruby -e '<<' 2>&1`
  assert_equal o, "-e:1:2: syntax error, unexpected tLSHFT\n"
  o = `bin/mruby -e '<<-' 2>&1`
  assert_equal o, "-e:1:3: syntax error, unexpected tLSHFT\n"
end

assert('regression for #1572') do
  script, bin = Tempfile.new('test.rb'), Tempfile.new('test.mrb')
  system "echo 'p \"ok\"' > #{script.path}"
  system "bin/mrbc -g -o #{bin.path} #{script.path}"
  o = `bin/mruby -b #{bin.path}`.strip
  assert_equal o, '"ok"'
end
