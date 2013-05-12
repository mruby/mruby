
assert('ObjectSpace.count_objects') do
    h = {}
    ObjectSpace.count_objects(h)
    assert_kind_of(Hash, h)
    assert_true(h.keys.all? {|x| x.is_a?(Symbol) || x.is_a?(Integer) })
    assert_true(h.values.all? {|x| x.is_a?(Integer) })

    h = ObjectSpace.count_objects
    assert_kind_of(Hash, h)
    assert_true(h.keys.all? {|x| x.is_a?(Symbol) || x.is_a?(Integer) })
    assert_true(h.values.all? {|x| x.is_a?(Integer) })

    assert_raise(TypeError) { ObjectSpace.count_objects(1) }

    h0 = {:MRB_TT_FOO=>1000}
    h = ObjectSpace.count_objects(h0)
    assert_false(h0.has_key?(:MRB_TT_FOO))
  end