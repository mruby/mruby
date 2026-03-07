##
# ENV test

assert('ENV class') do
  assert_equal Object, ENV.class
end

assert('ENV.to_s') do
  assert_equal "ENV", ENV.to_s
end

assert('ENV[] and ENV[]=') do
  ENV["MRUBY_ENV_TEST_A"] = "hello"
  assert_equal "hello", ENV["MRUBY_ENV_TEST_A"]
  ENV["MRUBY_ENV_TEST_A"] = nil
  assert_nil ENV["MRUBY_ENV_TEST_A"]
end

assert('ENV[] returns nil for missing key') do
  assert_nil ENV["MRUBY_ENV_TEST_NONEXISTENT"]
end

assert('ENV.store') do
  ENV.store("MRUBY_ENV_TEST_B", "world")
  assert_equal "world", ENV["MRUBY_ENV_TEST_B"]
  ENV.delete("MRUBY_ENV_TEST_B")
end

assert('ENV.delete') do
  ENV["MRUBY_ENV_TEST_C"] = "val"
  assert_equal "val", ENV.delete("MRUBY_ENV_TEST_C")
  assert_nil ENV["MRUBY_ENV_TEST_C"]
end

assert('ENV.delete returns nil for missing key') do
  assert_nil ENV.delete("MRUBY_ENV_TEST_NONEXISTENT")
end

assert('ENV.delete with block') do
  result = ENV.delete("MRUBY_ENV_TEST_NONEXISTENT") { |k| "missing: #{k}" }
  assert_equal "missing: MRUBY_ENV_TEST_NONEXISTENT", result
end

assert('ENV.key?') do
  ENV["MRUBY_ENV_TEST_D"] = "yes"
  assert_true ENV.key?("MRUBY_ENV_TEST_D")
  assert_false ENV.key?("MRUBY_ENV_TEST_NONEXISTENT")
  ENV.delete("MRUBY_ENV_TEST_D")
end

assert('ENV.include?/has_key?/member?') do
  ENV["MRUBY_ENV_TEST_E"] = "val"
  assert_true ENV.include?("MRUBY_ENV_TEST_E")
  assert_true ENV.has_key?("MRUBY_ENV_TEST_E")
  assert_true ENV.member?("MRUBY_ENV_TEST_E")
  ENV.delete("MRUBY_ENV_TEST_E")
end

assert('ENV.keys') do
  ENV["MRUBY_ENV_TEST_F"] = "val"
  k = ENV.keys
  assert_kind_of Array, k
  assert_true k.include?("MRUBY_ENV_TEST_F")
  ENV.delete("MRUBY_ENV_TEST_F")
end

assert('ENV.values') do
  ENV["MRUBY_ENV_TEST_G"] = "unique_val_42"
  v = ENV.values
  assert_kind_of Array, v
  assert_true v.include?("unique_val_42")
  ENV.delete("MRUBY_ENV_TEST_G")
end

assert('ENV.size/length') do
  s1 = ENV.size
  ENV["MRUBY_ENV_TEST_H"] = "val"
  assert_equal s1 + 1, ENV.size
  assert_equal ENV.size, ENV.length
  ENV.delete("MRUBY_ENV_TEST_H")
end

assert('ENV.empty?') do
  assert_equal [true, false].include?(ENV.empty?), true
end

assert('ENV.has_value?/value?') do
  ENV["MRUBY_ENV_TEST_I"] = "unique_find_me"
  assert_true ENV.has_value?("unique_find_me")
  assert_true ENV.value?("unique_find_me")
  assert_false ENV.has_value?("MRUBY_ENV_NEVER_EXISTS_VALUE")
  ENV.delete("MRUBY_ENV_TEST_I")
end

assert('ENV.each/each_pair') do
  ENV["MRUBY_ENV_TEST_J"] = "iter_val"
  found = false
  ENV.each do |k, v|
    if k == "MRUBY_ENV_TEST_J" && v == "iter_val"
      found = true
    end
  end
  assert_true found
  ENV.delete("MRUBY_ENV_TEST_J")
end

assert('ENV.each_key') do
  ENV["MRUBY_ENV_TEST_K"] = "val"
  found = false
  ENV.each_key { |k| found = true if k == "MRUBY_ENV_TEST_K" }
  assert_true found
  ENV.delete("MRUBY_ENV_TEST_K")
end

assert('ENV.each_value') do
  ENV["MRUBY_ENV_TEST_L"] = "val_each_v"
  found = false
  ENV.each_value { |v| found = true if v == "val_each_v" }
  assert_true found
  ENV.delete("MRUBY_ENV_TEST_L")
end

assert('ENV.fetch') do
  ENV["MRUBY_ENV_TEST_M"] = "fetch_val"
  assert_equal "fetch_val", ENV.fetch("MRUBY_ENV_TEST_M")
  assert_equal "default", ENV.fetch("MRUBY_ENV_TEST_NONE", "default")
  assert_equal "block", ENV.fetch("MRUBY_ENV_TEST_NONE") { "block" }
  assert_raise(KeyError) { ENV.fetch("MRUBY_ENV_TEST_NONE") }
  ENV.delete("MRUBY_ENV_TEST_M")
end

assert('ENV.to_h') do
  ENV["MRUBY_ENV_TEST_N"] = "to_h_val"
  h = ENV.to_h
  assert_kind_of Hash, h
  assert_equal "to_h_val", h["MRUBY_ENV_TEST_N"]
  ENV.delete("MRUBY_ENV_TEST_N")
end

assert('ENV.to_a') do
  ENV["MRUBY_ENV_TEST_O"] = "to_a_val"
  a = ENV.to_a
  assert_kind_of Array, a
  assert_true a.include?(["MRUBY_ENV_TEST_O", "to_a_val"])
  ENV.delete("MRUBY_ENV_TEST_O")
end

assert('ENV.assoc') do
  ENV["MRUBY_ENV_TEST_P"] = "assoc_val"
  assert_equal ["MRUBY_ENV_TEST_P", "assoc_val"], ENV.assoc("MRUBY_ENV_TEST_P")
  assert_nil ENV.assoc("MRUBY_ENV_TEST_NONEXISTENT")
  ENV.delete("MRUBY_ENV_TEST_P")
end

assert('ENV.rassoc') do
  ENV["MRUBY_ENV_TEST_Q"] = "rassoc_unique_val"
  result = ENV.rassoc("rassoc_unique_val")
  assert_equal "MRUBY_ENV_TEST_Q", result[0]
  assert_equal "rassoc_unique_val", result[1]
  assert_nil ENV.rassoc("MRUBY_ENV_NEVER_EXISTS_VALUE")
  ENV.delete("MRUBY_ENV_TEST_Q")
end

assert('ENV.key') do
  ENV["MRUBY_ENV_TEST_R"] = "key_search_val"
  assert_equal "MRUBY_ENV_TEST_R", ENV.key("key_search_val")
  assert_nil ENV.key("MRUBY_ENV_NEVER_EXISTS_VALUE")
  ENV.delete("MRUBY_ENV_TEST_R")
end

assert('ENV.inspect') do
  s = ENV.inspect
  assert_kind_of String, s
end

assert('ENV.select/filter') do
  ENV["MRUBY_ENV_TEST_S"] = "sel_val"
  result = ENV.select { |k, v| k == "MRUBY_ENV_TEST_S" }
  assert_kind_of Hash, result
  assert_equal "sel_val", result["MRUBY_ENV_TEST_S"]
  ENV.delete("MRUBY_ENV_TEST_S")
end

assert('ENV.reject') do
  ENV["MRUBY_ENV_TEST_T"] = "rej_val"
  result = ENV.reject { |k, v| k != "MRUBY_ENV_TEST_T" }
  assert_equal({"MRUBY_ENV_TEST_T" => "rej_val"}, result)
  ENV.delete("MRUBY_ENV_TEST_T")
end

assert('ENV.replace') do
  ENV["MRUBY_ENV_TEST_U1"] = "old1"
  ENV.replace({"MRUBY_ENV_TEST_U2" => "new2"})
  assert_nil ENV["MRUBY_ENV_TEST_U1"]
  assert_equal "new2", ENV["MRUBY_ENV_TEST_U2"]
  ENV.delete("MRUBY_ENV_TEST_U2")
end

assert('ENV.update/merge!') do
  ENV["MRUBY_ENV_TEST_V"] = "orig"
  ENV.update({"MRUBY_ENV_TEST_V" => "updated", "MRUBY_ENV_TEST_W" => "new"})
  assert_equal "updated", ENV["MRUBY_ENV_TEST_V"]
  assert_equal "new", ENV["MRUBY_ENV_TEST_W"]
  ENV.delete("MRUBY_ENV_TEST_V")
  ENV.delete("MRUBY_ENV_TEST_W")
end

assert('ENV.update with block') do
  ENV["MRUBY_ENV_TEST_X"] = "old"
  ENV.update({"MRUBY_ENV_TEST_X" => "new"}) { |k, o, n| "#{o}_#{n}" }
  assert_equal "old_new", ENV["MRUBY_ENV_TEST_X"]
  ENV.delete("MRUBY_ENV_TEST_X")
end

assert('ENV.slice') do
  ENV["MRUBY_ENV_TEST_Y"] = "s1"
  ENV["MRUBY_ENV_TEST_Z"] = "s2"
  h = ENV.slice("MRUBY_ENV_TEST_Y", "MRUBY_ENV_TEST_Z", "MRUBY_ENV_TEST_NONEXISTENT")
  assert_equal({"MRUBY_ENV_TEST_Y" => "s1", "MRUBY_ENV_TEST_Z" => "s2"}, h)
  ENV.delete("MRUBY_ENV_TEST_Y")
  ENV.delete("MRUBY_ENV_TEST_Z")
end

assert('ENV.freeze raises TypeError') do
  assert_raise(TypeError) { ENV.freeze }
end

assert('ENV raises TypeError for non-string key') do
  assert_raise(TypeError) { ENV[123] }
end

assert('ENV raises TypeError for non-string value') do
  assert_raise(TypeError) { ENV["MRUBY_ENV_TEST_ERR"] = 123 }
end

assert('ENV.each returns enumerator without block') do
  e = ENV.each
  assert_kind_of Enumerator, e
end

assert('ENV is Enumerable') do
  assert_true ENV.is_a?(Enumerable)
end
