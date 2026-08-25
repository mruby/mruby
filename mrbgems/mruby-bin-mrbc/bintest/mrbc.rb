require 'open3'
require 'tempfile'
require 'tmpdir'

assert('Compiling multiple files without new line in last line. #2361') do
  a, b, out = Tempfile.new('a.rb'), Tempfile.new('b.rb'), Tempfile.new('out.mrb')
  a.write('module A; end')
  a.flush
  b.write('module B; end')
  b.flush
  result, status = Open3.capture2e(*(cmd_list('mrbc') + ['-c', '-o', out.path, a.path, b.path]))
  assert_equal "#{cmd_bin('mrbc')}:#{a.path}:Syntax OK", result.chomp
  assert_equal 0, status.exitstatus
end

assert('parsing function with void argument') do
  a, out = Tempfile.new('a.rb'), Tempfile.new('out.mrb')
  a.write('f ()')
  a.flush
  result, status = Open3.capture2e(*(cmd_list('mrbc') + ['-c', '-o', out.path, a.path]))
  assert_equal "#{cmd_bin('mrbc')}:#{a.path}:Syntax OK", result.chomp
  assert_equal 0, status.exitstatus
end

assert('embedded document with invalid terminator') do
  a, out = Tempfile.new('a.rb'), Tempfile.new('out.mrb')
  a.write("=begin\n=endx\n")
  a.flush
  result, status = Open3.capture2e(*(cmd_list('mrbc') + ['-c', '-o', out.path, a.path]))
  assert_equal "#{a.path}:2:1: syntax error, embedded document meets end of file", result.chomp
  assert_equal 1, status.exitstatus
end

assert('a float literal under MRB_NO_FLOAT is read as 0 with a warning') do
  # Only a build without Float takes this path.  Whether this is one is asked
  # of its mruby, when there is one; mrbc itself cannot be asked.
  skip 'no mruby to probe the build with' unless File.exist?(cmd_bin('mruby'))
  system(*(cmd_list('mruby') + ['-e', 'Float']), out: File::NULL, err: File::NULL)
  skip 'this build has Float' if $?.success?

  a, out = Tempfile.new('a.rb'), Tempfile.new('out.mrb')
  a.write("x = 1\np 1.5\n")
  a.flush
  result, status = Open3.capture2e(*(cmd_list('mrbc') + ['-v', '-o', out.path, a.path]))
  assert_equal 0, status.exitstatus
  assert_include result, "#{a.path}:2:3: generator warning, floating-point numbers are not supported"
  compiled, = Open3.capture2(*(cmd_list('mruby') + ['-b', out.path]))
  assert_equal "0\n", compiled
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

  # capture2 takes stdout only, which is where both write the disassembly.
  from_mrbc = clean.call(Open3.capture2(*(cmd_list('mrbc') + ['-v', '-o', out.path, a.path]))[0])
  from_mruby = clean.call(Open3.capture2(*(cmd_list('mruby') + ['-v', '-c', a.path]))[0])

  assert_false from_mrbc.empty?, 'mrbc -v produced no disassembly'
  assert_equal from_mruby, from_mrbc
end

assert('non-seekable input file is rejected by size, not blamed on the read') do
  # The file arm of the source reader sizes its buffer from ftell(). On a pipe
  # ftell() fails, and before it was checked the -1 propagated into the
  # allocation and the fread() count, surfacing as the misleading "cannot read
  # program file"; the file opens and reads fine, only its size is unknown.
  # Needs a genuinely unseekable path: `< file` would still be seekable.
  skip 'no /dev/stdin' if target_win?
  skip 'no /dev/stdin' unless File.exist?('/dev/stdin')

  a = Tempfile.new('a.rb')
  a.write("puts 1\n")
  a.flush

  # The one command here that a shell has to read: what is under test is what
  # arrives through a pipe, and the pipeline is the shell's to build.  The
  # path is quoted for it.
  result = `cat #{shellquote(a.path)} | #{cmd('mrbc')} -c /dev/stdin 2>&1`
  assert_equal 1, $?.exitstatus
  assert_include result, 'compile.c: cannot get size of program file. (/dev/stdin)'
  assert_not_include result, 'cannot read program file'
end

assert('a directory as an input file is refused') do
  # Only POSIX systems open a directory for reading; Windows refuses it at
  # fopen() and never reaches the reader this guards.
  skip 'fopen() refuses a directory' if target_win?
  # ftell() answers LONG_MAX for a directory stream on ext4 and 0 on tmpfs,
  # and the size check accepts both: the first overflows the length
  # arithmetic that sizes the buffer, the second compiles as an empty
  # program.  The fread() failure below reports the LONG_MAX case in wording
  # of its own, so pin which message arrives, not merely that one did.
  Dir.mktmpdir do |dir|
    result, status = Open3.capture2e(*(cmd_list('mrbc') + ['-c', dir]))
    assert_include result, 'compile.c: cannot read from program file.'
    assert_not_include result, 'compile.c: cannot read program file.'
    assert_equal 1, status.exitstatus
  end
end

assert('a super outside a method forwards no block') do
  # The walk for the block a bare `super` forwards stops at the enclosing
  # method scope. Outside one there is none, and the walk used to hand
  # OP_GETUPVAR the count it had reached, a level past the outermost scope
  # that names nothing (#7290). The block to forward is nil there.
  src = <<~'EOS'
    super
    [1].each { super }
    -> { super }
    class C
      super
    end
  EOS
  a, out = Tempfile.new('a.rb'), Tempfile.new('out.mrb')
  a.write(src)
  a.flush
  result, status = Open3.capture2e(*(cmd_list('mrbc') + ['-v', '-o', out.path, a.path]))
  assert_equal 0, status.exitstatus
  assert_include result, 'SUPER'
  assert_not_include result, 'GETUPVAR'
end
