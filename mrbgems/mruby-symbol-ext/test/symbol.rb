##
# Symbol(Ext) Test

assert('Symbol#to_proc') do
  assert_equal 5, :abs.to_proc[-5]

  o = Object.new
  def o.m_block_given?
    block_given?
  end
  assert_true :m_block_given?.to_proc.call(o){}
  assert_false :m_block_given?.to_proc.call(o)
end

assert('Symbol.all_symbols') do
  foo = [:__symbol_test_1, :__symbol_test_2, :__symbol_test_3].sort
  symbols = Symbol.all_symbols.select{|sym|sym.to_s.include? '__symbol_test'}.sort
  assert_equal foo, symbols
end

assert("Symbol#length") do
  assert_equal 5, :hello.size
  assert_equal 5, :mruby.length
end

assert("Symbol#capitalize") do
  assert_equal :Hello, :hello.capitalize
  assert_equal :Hello, :HELLO.capitalize
end

assert("Symbol#downcase") do
  assert_equal :hello, :hEllO.downcase
end

assert("Symbol#upcase") do
  assert_equal :HELLO, :hEllO.upcase
end

assert("Symbol#empty?") do
  assert_true :''.empty?
end
