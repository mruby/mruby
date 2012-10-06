if self.methods.include?(:require) and self.methods.include?(:load) and
  Object.const_defined?(:File) and Object.const_defined?(:IO)

  assert('load with nil') do
    e = nil
    begin
      load nil
    rescue => e
    end
    e.class == TypeError
  end

  assert('require with nil') do
    e = nil
    begin
      require nil
    rescue => e
    end
    e.class == TypeError
  end

  assert('$: should be empty array.') do
    $:.class == Array and
    $:.size == 0
  end

  assert('$" should be empty array.') do
    $".class == Array and
    $".size == 0
  end


  assert('load "notfound" should not success') do
    $: = ["."]
    e = nil
    begin
      load "notfound"
    rescue LoadError => e
    end
    e.class == LoadError
  end

  assert('require "notfound" should not success') do
    $: = ["."]
    $" = []
    e = nil
    begin
      require "notfound"
    rescue LoadError => e
    end
    e.class == LoadError and $" == []
  end

  assert('load "mrb0" should be success') do
    $: = ["require"]
    $" = []
    e = nil
    before = Object.const_defined?(:Mrb0)
    begin
      load "mrb0"
    rescue LoadError => e
    end
    after = Object.const_defined?(:Mrb0)
    e == nil and $" == [] and before == false and after == true and $mrb0_filename == "require/mrb0_.rb"
  end

  assert('require "mrb1" should be success') do
    $: = ["require"]
    $" = []
    e = nil
    mrb1 = 0
    before = Object.const_defined?(:Mrb1)
    begin
      t = require "mrb1"
    rescue LoadError=> e
    end
    after = Object.const_defined?(:Mrb1)
    e == nil and $" == ["require/mrb1.mrb"] and before == false and after == true and mrb1 == 0 and t == true
  end

  assert('require "mrb1" should not reloaded') do
    before = ($" == ["require/mrb1.mrb"])
    e = nil
    begin
      t = require "mrb1"
    rescue LoadError => e
    end
    e == nil and before == true and $" == ["require/mrb1.mrb"] and t == false
  end

  assert('load "rb0" should be success') do
    e = nil
    before = Object.const_defined?(:Rb0)
    begin
      load "rb0"
    rescue LoadError => e
    end
    after = Object.const_defined?(:Rb0)
    e == nil and before == false and after == true and $rb0_filename == "require/rb0.rb"
  end

  assert('require "rb0" should be success') do
    e = nil
    begin
      t = require "rb0"
    rescue LoadError => e
    end
    e == nil and t == true and $".include?("require/rb0.rb")
  end

  assert('require "rb1" should be failed') do
    e = nil
    before = Object.const_defined?(:Rb1)
    begin
      require "rb1"
    rescue LoadError => e
    end
    after = Object.const_defined?(:Rb1)
    e.class == LoadError and before == false and after == false
  end

  assert('require "rb2" should be success') do
    e = nil
    before1 = Object.const_defined?(:Rb1)
    before2 = Object.const_defined?(:Rb2)
    begin
      require "rb2"
    rescue LoadError => e
    end
    after1 = Object.const_defined?(:Rb1)
    after2 = Object.const_defined?(:Rb2)

    e == nil and before1 == false and before2 == false and
    after1 == true and after2 == true and 
    $".include?("require/rb2.rb") and
    $".include?("require/dir/rb1.rb")
  end

  assert('$0 and __FILE__ check') do
    e = nil
    before = Object.const_defined?(:Rb3)
    begin
      load "rb3"
    rescue LoadError => e
    end
    after = Object.const_defined?(:Rb3)

    e == nil and before == false and after == true and
    $rb3_exec == nil and $rb3_not_exec == true
  end

  assert('require "rb4" should be RuntimeError') do
    e = nil
    e2 = nil
    begin
      require "rb4"
    rescue LoadError => e
    rescue => e2
    end

    e == nil and e2.class == RuntimeError and $rb4 == true
  end

  assert('require "rb5" should be RequireTestError') do
    e = nil
    e2 = nil
    begin
      require "rb5"
    rescue LoadError => e
    rescue => e2
    end

    e == nil and e2.class == RequireTestError and $rb5 == true
  end

  report

end

