assert "return throw value" do
  val = ["val"]
  result = catch :foo do
    loop do
      loop do
        throw :foo, val
        break
      end
      flunk("should not reach here")
    end
    false
  end
  assert_same(val, result)
end

assert "no throw" do
  assert_equal(:foo, catch(:bar){:foo})
end

assert "no throw value" do
  result = catch :foo do
    throw :foo
    1
  end
  assert_equal(nil, result)
end

assert "pass the given tag to block" do
  tag = [:foo]
  catch(tag){|t| assert_same(tag, t)}
end

assert "tag identity" do
  assert_raise_with_message_pattern(Exception, "uncaught throw *") do
    catch [:tag] do
      throw [:tag]
    end
    flunk("should not reach here")
  end
end

assert "without catch arguments" do
  result = catch do |tag1|
    catch do |tag2|
      throw tag1, 1
      flunk("should not reach here 1")
    end
    flunk("should not reach here 2")
  end
  assert_equal(1, result)
end

assert "catches across invocation boundaries" do
  v = []
  catch :one do
    v << 1
    catch :two do
      v << 2
      throw :one
      v << 3
    end
    v << 4
  end
  assert_equal([1,2], v)
end

assert "catches in the nested invocation with the same key" do
  v = []
  catch :tag do
    v << 1
    catch :tag do
      v << 2
      throw :tag
      v << 3
    end
    v << 4
  end
  assert_equal([1,2,4], v)
end
