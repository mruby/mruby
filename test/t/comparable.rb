
assert('Comparable#<', '15.3.3.2.1') do
  class Foo
    include Comparable
    def <=>(x)
      0
    end
  end

  (Foo.new < Foo.new) == false
end

assert('Comparable#<=', '15.3.3.2.2') do
  class Foo
    include Comparable
    def <=>(x)
      0
    end
  end

  (Foo.new <= Foo.new) == true
end

assert('Comparable#==', '15.3.3.2.3') do
  class Foo
    include Comparable
    def <=>(x)
      0
    end
  end

  (Foo.new == Foo.new) == true
end

assert('Comparable#>', '15.3.3.2.4') do
  class Foo
    include Comparable
    def <=>(x)
      0
    end
  end

  (Foo.new > Foo.new) == false
end

assert('Comparable#>=', '15.3.3.2.5') do
  class Foo
    include Comparable
    def <=>(x)
      0
    end
  end

  (Foo.new >= Foo.new) == true
end

assert('Comparable#between?', '15.3.3.2.6') do
  class Foo
    include Comparable
    def <=>(x)
      x
    end
  end

  c = Foo.new
  c.between?(-1,  1) == false &&
  c.between?(-1, -1) == false &&
  c.between?( 1,  1) == false &&
  c.between?( 1, -1) == true &&
  c.between?(0, 0) == true
end
