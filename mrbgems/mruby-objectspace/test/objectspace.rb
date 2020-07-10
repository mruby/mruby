# coding: utf-8
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

assert 'ObjectSpace.memsize_of' do
  # immediate literals
  int_size = ObjectSpace.memsize_of 1
  assert_equal int_size, 0, 'int zero'

  sym_size = ObjectSpace.memsize_of :foo
  assert_equal sym_size, 0, 'sym zero'

  assert_equal ObjectSpace.memsize_of(true), int_size
  assert_equal ObjectSpace.memsize_of(false), int_size

  float_size = if Object.const_defined? :Float
                 ObjectSpace.memsize_of 1.0
               else
                 nil
               end

  # need some way of asking if floats are boxed
  assert_equal float_size, 0 if float_size

  assert_not_equal ObjectSpace.memsize_of('a'), 0, 'memsize of str'

  if __ENCODING__ == "UTF-8"
    assert_not_equal ObjectSpace.memsize_of("こんにちは世界"), 0, 'memsize of utf8 str'
  end

  assert_not_equal ObjectSpace.memsize_of(0..1), 0, 'range not zero'

  # class defs
  class_obj_size = ObjectSpace.memsize_of Class
  assert_not_equal class_obj_size, 0, 'Class obj not zero'

  empty_class_def_size = ObjectSpace.memsize_of Class.new

  # need access to struct iv_tbl
  # assert_not_equal empty_class_def_size, 0, 'Class def not zero'

  class_without_methods = Class.new do
    @a = 1
    @b = 2
  end
  class_total_size = empty_class_def_size + (int_size * 2)
  assert_equal ObjectSpace.memsize_of(class_without_methods), class_total_size, 'class without methods size'

  module_without_methods = Module.new do
    @a = 1
    @b = 2
  end
  module_total_size = empty_class_def_size + (int_size * 2)
  assert_equal ObjectSpace.memsize_of(module_without_methods), module_total_size, 'module without methods size'

  proc_size = ObjectSpace.memsize_of Proc.new { x = 1; x }
  assert_not_equal proc_size, 0

  class_with_methods = Class.new do
    def foo
      a = 0
      a + 1
    end
  end

  m_size = ObjectSpace.memsize_of class_with_methods.instance_method(:foo)
  assert_not_equal m_size, 0, 'method size not zero'

  # collections
  assert_equal ObjectSpace.memsize_of([]), 0, 'empty array size zero'
  assert_not_equal ObjectSpace.memsize_of(Array.new(16)), 0, 'array size non zero'

  # fiber
  assert_not_equal ObjectSpace.memsize_of(Fiber.new {}), 0, 'fiber non zero'

  skip 'No hash table support yet'
  assert_equal ObjectSpace.memsize_of({}), 0, 'empty hash size zero'
end
