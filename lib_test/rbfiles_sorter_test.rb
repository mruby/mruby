require "minitest/autorun"

require "mruby/build/rbfiles_sorter"

class TestRbfilesSorter < Minitest::Test
  def test_sort
    @sorter = MRuby::RbfilesSorter.new("#{__dir__}/support/rbfiles_sorter")
    sorted = @sorter.sort

    root = File.expand_path("#{__dir__}/support/rbfiles_sorter")

    assert_equal "#{root}/stuff/y.rb", sorted[0]
    assert_equal "#{root}/utils/x.rb", sorted[1]
    assert_equal "#{root}/b.rb", sorted[2]
    assert_equal "#{root}/a.rb", sorted[3]
  end

  def test_sort_without_requires
    @sorter = MRuby::RbfilesSorter.new("#{__dir__}/support/rbfiles_sorter_2")
    sorted = @sorter.sort

    root = File.expand_path("#{__dir__}/support/rbfiles_sorter_2")

    assert_equal "#{root}/0_x.rb", sorted[0]
    assert_equal "#{root}/a.rb", sorted[1]
    assert_equal "#{root}/c.rb", sorted[2]
  end
end
