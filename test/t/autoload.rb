##
# Module#autoload Test

# Module#autoload loads with Kernel#require, which mruby itself does not
# have: this one, on the top-level self that autoload calls it on, runs the
# Proc registered for the path and records the path.
$autoload_files = {}
$autoload_required = []

def self.require(path)
  $autoload_required << path
  file = $autoload_files[path]
  raise RuntimeError, "cannot load such file -- #{path}" unless file
  file.call
  true
end

def autoload_file(path, &block)
  $autoload_files[path] = block
end

module AutoloadTest
end

assert('Module#autoload') do
  autoload_file("al/a") { module AutoloadTest; A = 1; end }
  module AutoloadTest; autoload :A, "al/a"; end

  assert_nil AutoloadTest.autoload(:A, "al/a")
  assert_false $autoload_required.include?("al/a")
  assert_equal 1, AutoloadTest::A
  assert_equal ["al/a"], $autoload_required.select { |path| path == "al/a" }
  assert_equal 1, AutoloadTest::A
  assert_equal ["al/a"], $autoload_required.select { |path| path == "al/a" }
end

assert('Module#autoload?') do
  autoload_file("al/p") { module AutoloadTest; P = 1; end }
  module AutoloadTest; autoload :P, "al/p"; end

  assert_equal "al/p", AutoloadTest.autoload?(:P)
  assert_nil AutoloadTest.autoload?(:Nothing)
  AutoloadTest::P
  assert_nil AutoloadTest.autoload?(:P)
end

assert('Module#autoload leaves a defined constant alone') do
  module AutoloadTest; DEFINED = :defined; autoload :DEFINED, "al/none"; end

  assert_equal :defined, AutoloadTest::DEFINED
  assert_nil AutoloadTest.autoload?(:DEFINED)
end

assert('Module#autoload checks its arguments') do
  assert_raise(NameError) { AutoloadTest.autoload(:lower, "al/x") }
  assert_raise(ArgumentError) { AutoloadTest.autoload(:Empty, "") }
  assert_raise(TypeError) { AutoloadTest.autoload(:NoPath, 1) }
end

assert('a pending autoload is defined without loading') do
  autoload_file("al/d") { module AutoloadTest; D = 1; end }
  module AutoloadTest; autoload :D, "al/d"; end

  assert_true AutoloadTest.const_defined?(:D)
  assert_true AutoloadTest.const_defined?(:D, false)
  assert_equal "constant", defined?(AutoloadTest::D)
  # Module#constants is mruby-metaprog's
  assert_true AutoloadTest.constants.include?(:D) if AutoloadTest.respond_to?(:constants)
  assert_false $autoload_required.include?("al/d")
end

assert('defined? loads the autoloads on the way to the last name') do
  autoload_file("al/outer") { module AutoloadTest; module Outer; autoload :Inner, "al/inner"; end; end }
  autoload_file("al/inner") { module AutoloadTest; module Outer; Inner = 1; end; end }
  module AutoloadTest; autoload :Outer, "al/outer"; end

  assert_equal "constant", defined?(AutoloadTest::Outer::Inner)
  assert_true $autoload_required.include?("al/outer")
  assert_false $autoload_required.include?("al/inner")
end

assert('Module#const_get loads an autoload') do
  autoload_file("al/g") { module AutoloadTest; G = 1; end }
  autoload_file("al/h") { module AutoloadTest; H = 2; end }
  module AutoloadTest; autoload :G, "al/g"; autoload :H, "al/h"; end

  assert_equal 1, AutoloadTest.const_get(:G)
  assert_equal 2, AutoloadTest.const_get(:H, false)
end

assert('an autoload shadows the same name further out') do
  autoload_file("al/s") { module AutoloadTest; module Shadow; class String; end; end; end }
  module AutoloadTest; module Shadow; autoload :String, "al/s"; end; end

  module AutoloadTest
    module Shadow
      assert_equal "AutoloadTest::Shadow::String", String.to_s
    end
  end
end

assert('an autoload in a superclass') do
  autoload_file("al/sup") { class AutoloadTest::Sup; X = :sup; end }
  class AutoloadTest::Sup; autoload :X, "al/sup"; end
  class AutoloadTest::Sub < AutoloadTest::Sup; end

  assert_equal :sup, AutoloadTest::Sub::X
end

assert('module and class reopen a pending autoload') do
  autoload_file("al/m") { module AutoloadTest; module M; def self.main; :loaded; end; end; end }
  autoload_file("al/c") { module AutoloadTest; class C; def self.main; :loaded; end; end; end }
  module AutoloadTest; autoload :M, "al/m"; autoload :C, "al/c"; end

  module AutoloadTest; module M; end; end
  class AutoloadTest::C; end
  assert_equal :loaded, AutoloadTest::M.main
  assert_equal :loaded, AutoloadTest::C.main
end

assert('a constant is not there while its file is loading') do
  autoload_file("al/l") do
    module AutoloadTest
      $autoload_loading = [defined?(L), const_defined?(:L), autoload?(:L)]
      class L; end
    end
  end
  module AutoloadTest; autoload :L, "al/l"; end

  assert_equal Class, AutoloadTest::L.class
  assert_equal [nil, false, nil], $autoload_loading
end

assert('an autoload whose file does not define the name') do
  autoload_file("al/u") { }
  module AutoloadTest; autoload :U, "al/u"; end

  assert_raise(NameError) { AutoloadTest::U }
  assert_false AutoloadTest.const_defined?(:U)
  assert_nil AutoloadTest.autoload?(:U)
end

assert('an autoload in an included module whose file does not define the name') do
  autoload_file("al/i") { }
  module AutoloadTest::Included; autoload :I, "al/i"; end
  class AutoloadTest::Includer; include AutoloadTest::Included; end

  assert_raise(NameError) { AutoloadTest::Includer::I }
  assert_raise(NameError) { AutoloadTest::Includer::I }
  assert_equal ["al/i"], $autoload_required.select { |path| path == "al/i" }
  assert_false AutoloadTest::Included.const_defined?(:I)
end

assert('an autoload in a frozen module whose file does not define the name') do
  autoload_file("al/f") { }
  module AutoloadTest::Frozen; autoload :F, "al/f"; end
  AutoloadTest::Frozen.freeze

  assert_raise(NameError) { AutoloadTest::Frozen::F }
  assert_false AutoloadTest::Frozen.const_defined?(:F)
end

assert('an autoload whose file raises stays') do
  $autoload_tries = 0
  autoload_file("al/r") do
    $autoload_tries += 1
    raise "broken" if $autoload_tries == 1
    module AutoloadTest; R = :r; end
  end
  module AutoloadTest; autoload :R, "al/r"; end

  assert_raise_with_message(RuntimeError, "broken") { AutoloadTest::R }
  assert_equal "al/r", AutoloadTest.autoload?(:R)
  assert_equal :r, AutoloadTest::R
end

assert('Module#remove_const of a pending autoload') do
  module AutoloadTest; autoload :Removed, "al/removed"; end

  assert_nil AutoloadTest.__send__(:remove_const, :Removed)
  assert_false AutoloadTest.const_defined?(:Removed)
end

assert('Kernel#autoload') do
  autoload_file("al/k") { module AutoloadTest; K = :k; end }
  module AutoloadTest
    autoload :K, "al/k"
    $autoload_kernel = autoload?(:K)
  end

  assert_equal "al/k", $autoload_kernel
  assert_equal "al/k", AutoloadTest.autoload?(:K)
  assert_equal :k, AutoloadTest::K
end

assert('an autoload whose file only autoloads the name anew') do
  autoload_file("al/again") { module AutoloadTest; autoload :Again, "al/again2"; end }
  module AutoloadTest; autoload :Again, "al/again"; end

  assert_raise(NameError) { AutoloadTest::Again }
  assert_equal "al/again2", AutoloadTest.autoload?(:Again)
end

assert('an autoload with no Kernel#require to load it') do
  class << self
    alias __autoload_test_require require
    undef require
  end
  begin
    skip "this build has a Kernel#require" if respond_to?(:require, true)
    module AutoloadTest; autoload :NoReq, "al/noreq"; end

    e = assert_raise(NotImplementedError) { AutoloadTest::NoReq }
    assert_include e.message, "Kernel#require"
    assert_include e.message, "AutoloadTest::NoReq"
    assert_equal "al/noreq", AutoloadTest.autoload?(:NoReq)
  ensure
    class << self
      alias require __autoload_test_require
    end
  end
end
