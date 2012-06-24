##
# Kernel ISO Test

assert('Kernel', '15.3.1') do
  Kernel.class == Module
end

assert('Kernel.block_given?', '15.3.1.2.2') do
  def bg_try(&b)
    if block_given?
      yield
    else
      "no block"
    end
  end
  (Kernel.block_given? == false) && (bg_try == "no block") && ((bg_try { "block" }) == "block") && ((bg_try do "block" end) == "block")
end

assert('Kernel.global_variables', '15.3.1.2.4') do
  Kernel.global_variables.class == Array
end

assert('Kernel.iterator?', '15.3.1.2.5') do
  Kernel.iterator? == false
end

assert('Kernel.lambda', '15.3.1.2.6') do
  l = Kernel.lambda do
    true
  end

  l.call and l.class == Proc
end

assert('Kernel.loop', '15.3.1.2.8') do
  i = 0

  Kernel.loop do
    i += 1
    break if i == 100
  end

  i == 100
end

assert('Kernel.p', '15.3.1.2.9') do
  # TODO search for a way to test p to stdio
  true
end

assert('Kernel.print', '15.3.1.2.10') do
  # TODO search for a way to test print to stdio
  true
end

assert('Kernel.puts', '15.3.1.2.11') do
  # TODO search for a way to test puts to stdio
  true
end

# TODO fails at the moment without arguments
assert('Kernel.raise', '15.3.1.2.12') do
  e_list = []

  begin
    raise RuntimeError.new
  rescue => e
    e_list << e
  end

  e_list[0].class == RuntimeError
end

assert('Kernel#clone', '15.3.1.3.8') do
  class KernelCloneTest
    def initialize
      @v = 0
    end

    def get
      @v
    end

    def set(v)
      @v = v
    end
  end

  a = KernelCloneTest.new
  a.set(1)
  b = a.clone

  def a.test
  end
  a.set(2)
  c = a.clone

  a.get == 2 && b.get == 1 && c.get == 2 &&
    a.respond_to?(:test) == true && b.respond_to?(:test) == false && c.respond_to?(:test) == true
end

assert('Kernel#dup', '15.3.1.3.9') do
  class KernelDupTest
    def initialize
      @v = 0
    end

    def get
      @v
    end

    def set(v)
      @v = v
    end
  end

  a = KernelDupTest.new
  a.set(1)
  b = a.dup

  def a.test
  end
  a.set(2)
  c = a.dup

  a.get == 2 && b.get == 1 && c.get == 2 &&
    a.respond_to?(:test) == true && b.respond_to?(:test) == false && c.respond_to?(:test) == false
end

assert('Kernel#hash', '15.3.1.2.15') do
  hash == hash
end

assert('Kernel#loop', '15.3.1.2.29') do
  i = 0

  loop do
    i += 1
    break if i == 100
  end

  i == 100
end

assert('Kernel#methods', '15.3.1.2.31') do
  methods.class == Array
end

assert('Kernel#nil?', '15.3.1.2.32') do
  # TODO why is Kernel nil ????
  nil? == true
end

assert('Kernel#private_methods', '15.3.1.2.36') do
  private_methods.class == Array 
end

assert('Kernel#protected_methods', '15.3.1.2.37') do
  protected_methods.class == Array
end

assert('Kernel#public_methods', '15.3.1.2.38') do
  public_methods.class == Array
end

assert('Kernel#respond_to?', '15.3.1.2.43') do
  respond_to? :nil?
end

assert('Kernel#send', '15.3.1.2.44') do
  # test with block
  l = send(:lambda) do
    true
  end
  l.call and l.class == Proc and
    # test with argument
    send(:respond_to?, :nil?) and
    # test without argument and without block
    send(:public_methods).class == Array
end

assert('Kernel#singleton_methods', '15.3.1.2.45') do
  singleton_methods.class == Array
end

assert('Kernel#to_s', '15.3.1.2.46') do
  # TODO looks strange..
  to_s == ''
end
