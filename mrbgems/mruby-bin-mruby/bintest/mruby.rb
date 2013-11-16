assert('regression for #1564') do
  o = `bin/mruby -e '<<' 2>&1`
  assert_equal o, "-e:1:2: syntax error, unexpected tLSHFT\n"
  o = `bin/mruby -e '<<-' 2>&1`
  assert_equal o, "-e:1:3: syntax error, unexpected tLSHFT\n"
end
