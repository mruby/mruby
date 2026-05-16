require 'tempfile'

assert('Compiling multiple files without new line in last line. #2361') do
  a, b, out = Tempfile.new('a.rb'), Tempfile.new('b.rb'), Tempfile.new('out.mrb')
  a.write('module A; end')
  a.flush
  b.write('module B; end')
  b.flush
  result = `#{cmd('mrbc')} -c -o #{out.path} #{a.path} #{b.path} 2>&1`
  assert_equal "#{cmd_bin('mrbc')}:#{a.path}:Syntax OK", result.chomp
  assert_equal 0, $?.exitstatus
end

assert('parsing function with void argument') do
  a, out = Tempfile.new('a.rb'), Tempfile.new('out.mrb')
  a.write('f ()')
  a.flush
  result = `#{cmd('mrbc')} -c -o #{out.path} #{a.path} 2>&1`
  assert_equal "#{cmd_bin('mrbc')}:#{a.path}:Syntax OK", result.chomp
  assert_equal 0, $?.exitstatus
end

assert('embedded document with invalid terminator') do
  a, out = Tempfile.new('a.rb'), Tempfile.new('out.mrb')
  a.write("=begin\n=endx\n")
  a.flush
  result = `#{cmd('mrbc')} -c -o #{out.path} #{a.path} 2>&1`
  assert_equal "#{a.path}:3:0: embedded document meets end of file", result.chomp
  assert_equal 1, $?.exitstatus
end

assert('debug info preserves line/filename across multiple inputs. #1316') do
  # Verifying mrbc's debug info requires running the compiled output through
  # the mruby binary; skip when bin-mruby isn't built.  Kernel#puts only
  # exists when mruby-io is loaded, so the script uses print.
  skip "mruby command not built" unless File.exist?(cmd_bin("mruby"))

  a = Tempfile.new(['a', '.rb'])
  b = Tempfile.new(['b', '.rb'])
  out = Tempfile.new(['out', '.mrb'])
  a.write("# line 1\n# line 2\nprint \"from a\"\n# line 4\nundefined_in_a\n")
  a.flush
  b.write("# b line 1\nprint \"from b\"\n")
  b.flush
  `#{cmd('mrbc')} -g -o #{out.path} #{a.path} #{b.path}`
  assert_equal 0, $?.exitstatus
  result = `#{cmd('mruby')} -b #{out.path} 2>&1`
  # Error should point at a.rb line 5 (the `undefined_in_a` line),
  # not b.rb or a different line within a.rb.
  assert_include result, "#{a.path}:5:"
  assert_not_include result, b.path
end
