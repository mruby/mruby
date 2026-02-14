require "pathname"

cc_command = File.join(ENV["BUILD_DIR"], "bin/mruby-cc")
testdata = Pathname(__dir__).parent + "testdata"

assert "C compile" do
  system *%W(#{cc_command} #{testdata + "test1.c"})
  assert_true $?.success?
end

assert "C compile (failing)" do
  system *%W(#{cc_command} #{testdata + "test2.c"}), err: File::NULL
  assert_false $?.success?
end
