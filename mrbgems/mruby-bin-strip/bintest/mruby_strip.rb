require 'open3'
require 'tempfile'

assert('no files') do
  o, s = Open3.capture2e(*cmd_list('mruby-strip'))
  assert_equal 1, s.exitstatus
  assert_equal "no files to strip", o.split("\n")[0]
end

assert('file not found') do
  o, s = Open3.capture2e(*(cmd_list('mruby-strip') + ['not_found.mrb']))
  assert_equal 1, s.exitstatus
  assert_equal "can't open file for reading not_found.mrb\n", o
end

assert('not irep file') do
  t = Tempfile.new('script.rb')
  t.write 'p test\n'
  t.flush
  o, s = Open3.capture2e(*(cmd_list('mruby-strip') + [t.path]))
  assert_equal 1, s.exitstatus
  assert_equal "can't read irep file #{t.path}\n", o
end

assert('success') do
  script_file, compiled1, compiled2 =
    Tempfile.new('script.rb'), Tempfile.new('c1.mrb'), Tempfile.new('c2.mrb')
  script_file.write "p 'test'\n"
  script_file.flush
  system(*(cmd_list('mrbc') + ['-g', '-o', compiled1.path, script_file.path]))
  system(*(cmd_list('mrbc') + ['-g', '-o', compiled2.path, script_file.path]))

  o, s = Open3.capture2(*(cmd_list('mruby-strip') + [compiled1.path]))
  assert_equal 0, s.exitstatus
  assert_equal "", o
  from_source, = Open3.capture2(*(cmd_list('mruby') + [script_file.path]))
  from_stripped, = Open3.capture2(*(cmd_list('mruby') + [compiled1.path]))
  assert_equal from_source, from_stripped

  o, s = Open3.capture2(*(cmd_list('mruby-strip') + [compiled1.path, compiled2.path]))
  assert_equal 0, s.exitstatus
  assert_equal "", o
end

assert('check debug section') do
  script_file, with_debug, without_debug =
    Tempfile.new('script.rb'), Tempfile.new('c1.mrb'), Tempfile.new('c2.mrb')
  script_file.write "p 'test'\n"
  script_file.flush
  system(*(cmd_list('mrbc') + ['-o', without_debug.path, script_file.path]))
  system(*(cmd_list('mrbc') + ['-g', '-o', with_debug.path, script_file.path]))

  assert_true with_debug.size >= without_debug.size

  system(*(cmd_list('mruby-strip') + [with_debug.path]))
  assert_equal without_debug.size, with_debug.size
end

assert('check lv section') do
  script_file, with_lv, without_lv =
    Tempfile.new('script.rb'), Tempfile.new('c1.mrb'), Tempfile.new('c2.mrb')
  script_file.write <<EOS
a, b = 0, 1
a += b
p Kernel.local_variables
EOS
  script_file.flush
  system(*(cmd_list('mrbc') + ['-o', with_lv.path, script_file.path]))
  system(*(cmd_list('mrbc') + ['-o', without_lv.path, script_file.path]))

  system(*(cmd_list('mruby-strip') + ['-l', without_lv.path]))
  assert_true without_lv.size < with_lv.size
#
#  assert_equal '[:a, :b]', Open3.capture2(*(cmd_list('mruby') + ['-b', with_lv.path]))[0].chomp
#  assert_equal '[]', Open3.capture2(*(cmd_list('mruby') + ['-b', without_lv.path]))[0].chomp
end
