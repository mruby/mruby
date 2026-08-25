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
