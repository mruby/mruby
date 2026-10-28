assert('ObjectSpace.count_objects') do
  h = {}
  f = Fiber.new {} if Object.const_defined?(:Fiber)
  ObjectSpace.count_objects(h)
  assert_kind_of(Hash, h)
  assert_true(h.keys.all? {|x| x.is_a?(Symbol) || x.is_a?(Integer) })
  assert_true(h.values.all? {|x| x.is_a?(Integer) })

  assert_true(h.has_key?(:TOTAL))
  assert_true(h.has_key?(:FREE))
  assert_true(h.has_key?(:T_FIBER)) if Object.const_defined? :Fiber

  assert_equal(h[:TOTAL] * 2, h.values.reduce(:+))

  h = ObjectSpace.count_objects
  assert_kind_of(Hash, h)
  assert_true(h.keys.all? {|x| x.is_a?(Symbol) || x.is_a?(Integer) })
  assert_true(h.values.all? {|x| x.is_a?(Integer) })

  assert_raise(TypeError) { ObjectSpace.count_objects(1) }

  h0 = {:T_FOO=>1000}
  h = ObjectSpace.count_objects(h0)
  assert_false(h0.has_key?(:T_FOO))

  GC.start
  h_after = {}
  h_before = ObjectSpace.count_objects

  objs = []
  1000.times do
    objs << {}
  end
  ObjectSpace.count_objects(h)
  objs = nil
  GC.start
  ObjectSpace.count_objects(h_after)

  assert_equal(h[:T_HASH], h_before[:T_HASH] + 1000)
  assert_equal(h_after[:T_HASH], h_before[:T_HASH])
end

assert('ObjectSpace.each_object') do
  objs = []
  objs_count = ObjectSpace.each_object { |obj|
    objs << obj
  }
  assert_equal objs.length, objs_count

  arys = []
  arys_count = ObjectSpace.each_object(Array) { |obj|
    arys << obj
  }
  assert_equal arys.length, arys_count
  assert_true arys.length < objs.length
end

assert 'Check class pointer of ObjectSpace.each_object.' do
  assert_nothing_raised { ObjectSpace.each_object { |obj| !obj } }
end

assert('ObjectSpace.each_object does not reach the frames of Array#join') do
  # join keeps the arrays it is descending through in an array of its own;
  # an element's to_s must not be able to find it and rewrite a frame.
  c = Class.new do
    attr_accessor :outer
    def to_s
      ObjectSpace.each_object(Array) do |a|
        a[0] = "f" * 64 if a.size == 2 && a[0].equal?(@outer)
      end
      "e"
    end
  end
  e = c.new
  e.outer = [[e], "a", "b"]
  assert_equal("e,a,b", e.outer.join(","))
end

assert('ObjectSpace.each_object does not reach a message mrb_format() builds') do
  # The %v argument's to_s could find the half-built message and replace it,
  # or return it to be appended to itself after its buffer had moved.
  other = "B" * 32
  c = Class.new do
    def to_proc
      1
    end
    define_method(:to_s) do
      ObjectSpace.each_object(String) do |s|
        s.replace(other) if s.empty? && !s.frozen? && !s.equal?(other)
      end
      "C"
    end
  end
  msg = nil
  begin
    [].each(&c.new)
  rescue TypeError => e
    msg = e.message
  end
  assert_equal("C cannot be converted to Proc by #to_proc", msg)
  assert_equal("B" * 32, other)
end

class ObjectSpaceInspectTarget
  def initialize(a)
    @a = a
    @b = 1
  end
end

assert('ObjectSpace.each_object does not reach the string Kernel#inspect builds') do
  # An instance variable's inspect could swap in a buffer shared with a
  # frozen string, whose first byte inspect then wrote in place.
  donor = ("-" * 4000).freeze
  c = Class.new do
    define_method(:inspect) do
      ObjectSpace.each_object(String) do |s|
        s.replace(donor) if s[0, 26] == "#<ObjectSpaceInspectTarget" && !s.frozen?
      end
      ""
    end
  end
  s = ObjectSpaceInspectTarget.new(c.new).inspect
  assert_equal(String, s.class)
  assert_equal("#<ObjectSpaceInspectTarget:", s[0, 27])
  assert_equal(" @a=, @b=1>", s[-11, 11])
  assert_equal("-" * 4000, donor)
end

class ObjectSpaceWitness
  # Records every object of `klass` ObjectSpace yields while one of its
  # methods runs, so a test can ask whether a result under construction was
  # among them.
  attr_reader :seen
  def initialize(klass)
    @klass = klass
    @seen = []
  end
  def look
    ObjectSpace.each_object(@klass) { |o| @seen << o }
  end
  def seen?(obj)
    @seen.any? { |o| o.equal?(obj) }
  end
  def inspect
    look
    "W"
  end
  alias to_s inspect
  def ==(other)
    look
    false
  end
  def <=>(other)
    0
  end
end

assert('ObjectSpace.each_object does not reach the string Array#inspect builds') do
  w = ObjectSpaceWitness.new(String)
  s = [1, w].inspect
  assert_equal("[1, W]", s)
  assert_false(w.seen?(s))
end

assert('ObjectSpace.each_object does not reach the string Array#join builds') do
  w = ObjectSpaceWitness.new(String)
  s = [1, [w]].join("-")
  assert_equal("1-W", s)
  assert_false(w.seen?(s))
end

assert('ObjectSpace.each_object does not reach what Hash builds') do
  w = ObjectSpaceWitness.new(String)
  s = {1 => w}.inspect
  assert_equal("{1 => W}", s)
  assert_false(w.seen?(s))

  w = ObjectSpaceWitness.new(Hash)
  rest = nil
  case {w => 1, a: 2}
  in {a: 2, **r}
    rest = r
  end
  assert_equal(1, rest.size)
  assert_equal(Hash, rest.class)
  assert_false(w.seen?(rest))
end

assert('ObjectSpace.each_object does not reach the string Module#to_s builds') do
  w = $os_witness = ObjectSpaceWitness.new(String)
  c = Class.new
  def c.inspect
    $os_witness.inspect
  end
  s = (class << c; self; end).to_s
  $os_witness = nil
  assert_equal("#<Class:W>", s)
  assert_false(w.seen?(s))
end

assert('ObjectSpace.each_object does not reach the string Range#inspect builds') do
  w = ObjectSpaceWitness.new(String)
  s = Range.new(w, w).inspect
  assert_equal("W..W", s)
  assert_false(w.seen?(s))
end

assert('mrb_gc_register counts its registrations') do
  # One mrb_gc_unregister() undoes one mrb_gc_register(). Two owners of the
  # same object each hold it, so the first to let go leaves the second's hold
  # standing; and a registration made twice takes two calls to undo, so
  # nothing is left pinned by accident either.
  pairs = [[1, 1], [1, 0], [2, 1], [2, 2], [3, 3], [5, 2], [0, 1]]
  #         freed   pinned  pinned  freed   freed   pinned  freed
  assert_equal [false, true, true, false, false, true, false],
               ObjectSpace.__gc_root_survivors(pairs)
end
