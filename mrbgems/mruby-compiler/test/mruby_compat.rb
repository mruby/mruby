##
# Loading a source file that cannot be read

assert('mrb_load_file_cxt reports a stream it cannot read') do
  skip 'no stdio' unless respond_to?(:load_file_exc)

  # A directory opens for reading on POSIX and then fails every read with
  # EISDIR.  The reader answered NULL for that and set no exception, so
  # mrb_load_exec() returned an undefined value and the caller could not tell
  # the failure from an empty file: `mirb DIR`, `mrdb DIR` and `mrb -r DIR`
  # each ran nothing, said nothing and exited 0.  '.' is a directory wherever
  # the suite runs.
  exc = load_file_exc('.')
  skip 'fopen() refuses a directory' if exc.nil?

  assert_kind_of ScriptError, exc
  # `to_s`, not `message`: on a tree without the fix `exc` is false, and this
  # has to report a failed assertion rather than raise on the way there.
  assert_include exc.to_s, '.'
end

assert('an empty source file is not a read failure') do
  skip 'no stdio' unless respond_to?(:load_file_exc)

  # The counterpart the guard must not catch: end of input with no error.
  # `/dev/null` reads as an empty program, which is legal and silent.
  exc = load_file_exc('/dev/null')
  skip 'no /dev/null' if exc.nil?

  assert_false exc
end

##
# A float literal in bytecode, run where there is no Float

assert('a float pool entry loads where Float is missing, and runs only if reached') do
  # The literal becomes the float pool entry the helper makes; it is a
  # string here so that this file compiles under every build.
  src = '$__float_pool ? 1234567890123 : :skipped'
  $__float_pool = false
  r = __float_pool_roundtrip(src)
  skip 'no integer pool entry in this build' if r.nil?
  same, value = r
  # read back and dumped again, the entry comes out as it went in
  assert_true same
  # what does not reach the literal runs to the end
  assert_equal :skipped, value

  $__float_pool = true
  if Object.const_defined?(:Float)
    assert_equal "1.5", __float_pool_roundtrip(src)[1].to_s
  else
    assert_raise(NotImplementedError) { __float_pool_roundtrip(src) }
  end
end

##
# Compiler contexts that outlive each other

assert('a parser state freed first leaves a younger one intact') do
  # Everything Prism allocates for a parse comes from an arena that belongs
  # to the parse's compiler context.  The arena that was current was the
  # one given back, on the assumption that contexts are freed in the reverse
  # order of their making; a caller that keeps two parser states and frees
  # the older one first then had the younger one's tree, constant pool and
  # options freed under it, and generating code from it read freed memory.
  assert_equal 7, __parsers_outlive_each_other('1 + 2', 'x = 3; x + 4')
end
