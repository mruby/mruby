##
# ObjectSpace.define_finalizer and undefine_finalizer

$finalized = []

# A finalizer made where its object is not, since a block that sees the
# object keeps it alive
def finalizer_proc(tag)
  Proc.new { |id| $finalized << [tag, id] }
end

def finalizer_proc_nested
  Proc.new { |id|
    $finalized << [:outer, id]
    finalizer_object(:inner)
  }
end

# Objects made in a method of their own, so that no register of the test
# holds them once it returns
def finalizer_object(tag)
  o = Object.new
  ObjectSpace.define_finalizer(o, finalizer_proc(tag))
  o.object_id
end

def finalizer_object_undefined
  o = Object.new
  ObjectSpace.define_finalizer(o, finalizer_proc(:undefined))
  ObjectSpace.undefine_finalizer(o)
  nil
end

def finalizer_object_nested
  o = Object.new
  ObjectSpace.define_finalizer(o, finalizer_proc_nested)
  nil
end

assert("ObjectSpace.define_finalizer") do
  o = Object.new
  pr = Proc.new {}
  r = ObjectSpace.define_finalizer(o, pr)
  assert_equal 0, r[0]
  assert_same pr, r[1]
  r = ObjectSpace.define_finalizer(o) {}
  assert_equal 0, r[0]
  assert_kind_of Proc, r[1]

  callable = Object.new
  def callable.call(id); end
  assert_same callable, ObjectSpace.define_finalizer(o, callable)[1]

  assert_raise(ArgumentError) { ObjectSpace.define_finalizer(o, 1) }
  assert_raise(ArgumentError) { ObjectSpace.define_finalizer(o) }
  assert_raise(ArgumentError) { ObjectSpace.define_finalizer(1, pr) }
  assert_raise(ArgumentError) { ObjectSpace.define_finalizer(:a, pr) }
  assert_raise(ArgumentError) { ObjectSpace.define_finalizer(nil, pr) }
  assert_raise(FrozenError) { ObjectSpace.define_finalizer(Object.new.freeze, pr) }
  ObjectSpace.undefine_finalizer(o)
end

assert("ObjectSpace.undefine_finalizer") do
  o = Object.new
  assert_same o, ObjectSpace.undefine_finalizer(o)
  assert_raise(FrozenError) { ObjectSpace.undefine_finalizer(Object.new.freeze) }
  assert_raise(FrozenError) { ObjectSpace.undefine_finalizer(1) }
end

assert("ObjectSpace.define_finalizer calls back once the object is collected") do
  $finalized.clear
  id = finalizer_object(:a)
  finalizer_object_undefined
  GC.start
  assert_equal [[:a, id]], $finalized
  GC.start
  assert_equal [[:a, id]], $finalized
end

assert("ObjectSpace.define_finalizer adds a finalizer once") do
  $finalized.clear
  o = Object.new
  pr = Proc.new { |id| $finalized << id }
  ObjectSpace.define_finalizer(o, pr)
  ObjectSpace.define_finalizer(o, pr)
  ObjectSpace.define_finalizer(o, Proc.new { |id| $finalized << id })
  id = o.object_id
  o = nil
  GC.start
  assert_equal [id, id], $finalized
end

assert("a finalizer may define a finalizer") do
  $finalized.clear
  finalizer_object_nested
  GC.start
  GC.start
  assert_equal [:outer, :inner], $finalized.map { |tag, _| tag }
end

class FinalizerRaising
  def call(id) = raise("call")
  def inspect = raise("inspect")
end

def finalizer_object_raising
  o = Object.new
  ObjectSpace.define_finalizer(o, FinalizerRaising.new)
  ObjectSpace.define_finalizer(o, finalizer_proc(:after))
  nil
end

assert("a finalizer that raises leaves the others to run") do
  $finalized.clear
  finalizer_object_raising
  GC.start
  assert_equal [:after], $finalized.map { |tag, _| tag }
end
