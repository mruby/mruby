
assert('top-level super', 'github #185') do
  begin
    super do end
  rescue NoMethodError
    true
  else
    false
  end
end

assert('parsing error with super do end', 'github #185') do
  class Foo
    def run
      yield self
    end
  end

  class Bar < Foo
    def run
      super do
        yield self
      end
    end
  end

  Bar.new.run do
  end

  true
end

assert('||= doesn\'t work', 'github #253') do
  begin
    a ||= 1
    b &&= 1
  rescue NoMethodError
    false
  else
    true
  end
end

assert('"slice" of shared string returns invalid result.', 'github #256') do
  a = "12345".slice(1, 3)
  b = a.slice(0)
  "#{b}:" == "2:"
end

