require 'tempfile'
require 'open3'

assert_mruby = ->(exp_out_pattern, exp_err_pattern, exp_success, args) do
  out, err, stat = Open3.capture3(cmd("mruby"), *args)
  assert_match(exp_out_pattern, out, "standard output")
  assert_match(exp_err_pattern, err, "standard error")
  assert_equal(exp_success, stat.success?, "exit success?")
end

assert('regression for #1564') do
  assert_mruby.("", "-e:1:2: syntax error, *", false, %w[-e <<])
  assert_mruby.("", "-e:1:3: syntax error, *", false, %w[-e <<-])
end

assert('regression for #1572') do
  script, bin = Tempfile.new('test.rb'), Tempfile.new('test.mrb')
  File.write script.path, 'p "ok"'
  system "#{cmd('mrbc')} -g -o #{bin.path} #{script.path}"
  o = `#{cmd('mruby')} -b #{bin.path}`.strip
  assert_equal '"ok"', o
end

assert '$0 value' do
  script, bin = Tempfile.new('test.rb'), Tempfile.new('test.mrb')

  # .rb script
  script.write "p $0\n"
  script.flush
  assert_equal "\"#{script.path}\"", `#{cmd('mruby')} "#{script.path}"`.chomp

  # .mrb file
  `#{cmd('mrbc')} -o "#{bin.path}" "#{script.path}"`
  assert_equal "\"#{bin.path}\"", `#{cmd('mruby')} -b "#{bin.path}"`.chomp

  # one liner
  assert_equal '"-e"', `#{cmd('mruby')} -e #{shellquote('p $0')}`.chomp
end

assert('float literal') do
  script, bin = Tempfile.new('test.rb'), Tempfile.new('test.mrb')
  File.write script.path, 'p [3.21, 2e308.infinite?, -2e308.infinite?]'
  system "#{cmd('mrbc')} -g -o #{bin.path} #{script.path}"
  assert_equal "[3.21, 1, -1]", `#{cmd('mruby')} -b #{bin.path}`.chomp!
end

assert '__END__', '8.6' do
  script = Tempfile.new('test.rb')

  script.write <<EOS
p 'test'
  __END__ = 'fin'
p __END__
__END__
p 'legend'
EOS
  script.flush
  assert_equal "\"test\"\n\"fin\"\n", `#{cmd('mruby')} #{script.path}`
end

assert('garbage collecting built-in classes') do
  script = Tempfile.new('test.rb')

  script.write <<RUBY
NilClass = nil
GC.start
Array.dup
print nil.class.to_s
RUBY
  script.flush
  assert_equal "NilClass", `#{cmd('mruby')} #{script.path}`
  assert_equal 0, $?.exitstatus
end

assert('mruby -c option') do
  assert_mruby.("Syntax OK\n", "", true, ["-c", "-e", "p 1"])
  assert_mruby.("", "-e:1:7: syntax error, *", false, ["-c", "-e", "p 1; 1."])
end

assert('mruby -d option') do
  o = `#{cmd('mruby')} -e #{shellquote('p $DEBUG')}`
  assert_equal "false\n", o
  o = `#{cmd('mruby')} -d -e #{shellquote('p $DEBUG')}`
  assert_equal "true\n", o
end

assert('mruby -e option (no code specified)') do
  assert_mruby.("", "* No code specified for -e\n", false, %w[-e])
end

assert('mruby -h option') do
  assert_mruby.("Usage: *mruby*", "", true, %w[-h])
end

assert('mruby -r option') do
  lib = Tempfile.new('lib.rb')
  lib.write <<EOS
class Hoge
  def hoge
    :hoge
  end
end
EOS
  lib.flush

  script = Tempfile.new('test.rb')
  script.write <<EOS
print Hoge.new.hoge
EOS
  script.flush
  assert_equal 'hoge', `#{cmd('mruby')} -r #{lib.path} #{script.path}`
  assert_equal 0, $?.exitstatus

  assert_equal 'hogeClass', `#{cmd('mruby')} -r #{lib.path} -r #{script.path} -e #{shellquote('print Hoge.class')}`
  assert_equal 0, $?.exitstatus
end

assert('mruby -r option (no library specified)') do
  assert_mruby.("", "*: No library specified for -r\n", false, %w[-r])
end

assert('mruby -r option (file not found)') do
  assert_mruby.("", "*: Cannot open library file: *", false, %w[-r _no_exists_])
end

assert('mruby invalid short option') do
  assert_mruby.("", "*: invalid option -1 *", false, %w[-1])
end

assert('mruby invalid long option') do
  assert_mruby.("", "*: invalid option --longopt *", false, %w[--longopt])
end

assert('unhandled exception') do
  assert_mruby.("", "* EXCEPTION!*", false, %w[-e raise("EXCEPTION!")])
end

assert('program file not found') do
  assert_mruby.("", "*: Cannot open program file*", false, %w[_no_exists_])
end
