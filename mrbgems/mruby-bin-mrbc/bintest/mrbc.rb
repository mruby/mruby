require 'tempfile'
require 'tmpdir'

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
  assert_equal "#{a.path}:2:1: syntax error, embedded document meets end of file", result.chomp
  assert_equal 1, $?.exitstatus
end

assert('a float literal under MRB_NO_FLOAT is read as 0 with a warning') do
  # Only a build without Float takes this path.  Whether this is one is asked
  # of its mruby, when there is one; mrbc itself cannot be asked.
  skip 'no mruby to probe the build with' unless File.exist?(cmd_bin('mruby'))
  system("#{cmd('mruby')} -e Float", out: File::NULL, err: File::NULL)
  skip 'this build has Float' if $?.success?

  a, out = Tempfile.new('a.rb'), Tempfile.new('out.mrb')
  a.write("x = 1\np 1.5\n")
  a.flush
  result = `#{cmd('mrbc')} -v -o #{out.path} #{a.path} 2>&1`
  assert_equal 0, $?.exitstatus
  assert_include result, "#{a.path}:2:3: generator warning, floating-point numbers are not supported"
  assert_equal "0\n", `#{cmd('mruby')} -b #{out.path}`
end

assert('mrbc -v disassembles like mruby -v') do
  # mruby-compiler carries its own copy of the disassembler, because it has to
  # build for mruby/c as well and cannot share src/codedump.c. The copy has
  # drifted before (see #6970), so pin the two outputs to each other.
  src = <<~'EOS'
    CONST = 1
    $gv = "s"
    class C
      @@cv = 2
      def initialize(a, b = 1, *r, k: 2, **kw, &blk)
        @iv = a
        @x = a + b - 1 * 2 / 3
        @y = a[0]
        @y[1] = 2
        @s = "lit#{a}" + ''
        @h = {x: 1, **kw}
        @ary = [*r, 1, 2]
        @cmp = (a < b) && (a > b) || (a <= b) && (a >= b) && (a == b)
      end
      def self.m = C::CONST
      protected def prot = @iv
    end
    module M; end
    [1, 2].each { |v| p v }
    begin
      raise "x"
    rescue => e
      p e
      retry if false
    ensure
      $gv = "done"
    end
    ->(z) { z }.call(1)
    case 1 when 1 then 2 else 3 end
    while false; break; end
    def kw(a:, b: 1); [a, b]; end
  EOS

  a, out = Tempfile.new('a.rb'), Tempfile.new('out.mrb')
  a.write(src)
  a.flush

  # Keep only the disassembly. The irep address has to go by position, not by
  # shape: MSVC prints "%p" as 000001C3D1A77D50 and glibc as 0x5b5cc80b2660.
  clean = lambda do |s|
    s.sub(/\A.*?^(?=irep )/m, '')
     .gsub(/^irep \S+ /, 'irep ADDR ')
     .lines.reject { |l| l.start_with?('Syntax OK') }.join
  end

  # Backticks capture stdout only, which is where both write the disassembly.
  # Do not redirect stderr: `2>/dev/null` is not portable to cmd.exe.
  from_mrbc = clean.call(`#{cmd('mrbc')} -v -o #{out.path} #{a.path}`)
  from_mruby = clean.call(`#{cmd('mruby')} -v -c #{a.path}`)

  assert_false from_mrbc.empty?, 'mrbc -v produced no disassembly'
  assert_equal from_mruby, from_mrbc
end

assert('non-seekable input file is rejected by size, not blamed on the read') do
  # The file arm of the source reader sizes its buffer from ftell(). On a pipe
  # ftell() fails, and before it was checked the -1 propagated into the
  # allocation and the fread() count, surfacing as the misleading "cannot read
  # program file"; the file opens and reads fine, only its size is unknown.
  # Needs a genuinely unseekable path: `< file` would still be seekable.
  skip 'no /dev/stdin' if /mswin(?!ce)|mingw|bccwin/ =~ RbConfig::CONFIG['host_os']
  skip 'no /dev/stdin' unless File.exist?('/dev/stdin')

  a = Tempfile.new('a.rb')
  a.write("puts 1\n")
  a.flush

  result = `cat #{shellquote(a.path)} | #{cmd('mrbc')} -c /dev/stdin 2>&1`
  assert_equal 1, $?.exitstatus
  assert_include result, 'compile.c: cannot get size of program file. (/dev/stdin)'
  assert_not_include result, 'cannot read program file'
end

assert('a directory as an input file is refused') do
  # Only POSIX systems open a directory for reading; Windows refuses it at
  # fopen() and never reaches the reader this guards.
  skip 'fopen() refuses a directory' if /mswin(?!ce)|mingw|bccwin/ =~ RbConfig::CONFIG['host_os']
  # ftell() answers LONG_MAX for a directory stream on ext4 and 0 on tmpfs,
  # and the size check accepts both: the first overflows the length
  # arithmetic that sizes the buffer, the second compiles as an empty
  # program.  The fread() failure below reports the LONG_MAX case in wording
  # of its own, so pin which message arrives, not merely that one did.
  Dir.mktmpdir do |dir|
    result = `#{cmd('mrbc')} -c #{shellquote(dir)} 2>&1`
    assert_include result, 'compile.c: cannot read from program file.'
    assert_not_include result, 'compile.c: cannot read program file.'
    assert_equal 1, $?.exitstatus
  end
end
