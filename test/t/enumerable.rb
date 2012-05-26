##
# Enumerable ISO Test

assert('Enumerable', '15.3.2') do
  Enumerable.class == Module
end

assert('Enumerable#all?', '15.3.2.2.1') do
  [1,2,3].all? and not [1,false,3].all?
end

assert('Enumerable#any?', '15.3.2.2.2') do
  [false,true,false].any? and not [false,false,false].any?
end

assert('Enumerable#collect', '15.3.2.2.3') do
  [1,2,3].collect { |i| i + i } == [2,4,6]
end

assert('Enumerable#detect', '15.3.2.2.4') do
  [1,2,3].detect() { true } and [1,2,3].detect("a") { false } == 'a'
end

assert('Array#each_with_index', '15.3.2.2.5') do
  a = nil
  b = nil

  [1].each_with_index {|e,i| a = e; b = i}

  a == 1 and b == 0
end

assert('Enumerable#entries', '15.3.2.2.6') do
  [1].entries == [1]
end

assert('Enumerable#find', '15.3.2.2.7') do
  [1,2,3].find() { true } and [1,2,3].find("a") { false } == 'a'
end

assert('Enumerable#find_all', '15.3.2.2.8') do
  [1,2,3,4,5,6,7,8,9].find_all() {|i| i%2 == 0} == [2,4,6,8]
end

assert('Enumerable#grep', '15.3.2.2.9') do
  [1,2,3,4,5,6,7,8,9].grep(4..6) == [4,5,6]
end

assert('Enumerable#include?', '15.3.2.2.10') do
  [1,2,3,4,5,6,7,8,9].include?(5) and
    not [1,2,3,4,5,6,7,8,9].include?(0)
end

assert('Enumerable#inject', '15.3.2.2.11') do
  [1,2,3,4,5,6].inject() {|s, n| s + n} == 21 and
    [1,2,3,4,5,6].inject(1) {|s, n| s + n} == 22
end

assert('Enumerable#map', '15.3.2.2.12') do
  [1,2,3].map { |i| i + i } == [2,4,6]
end

assert('Enumerable#max', '15.3.2.2.13') do
  a = ['aaa', 'bb', 'c']
  a.max == 'c' and
    a.max {|i1,i2| i1.length <=> i2.length} == 'aaa'
end

assert('Enumerable#min', '15.3.2.2.14') do
  a = ['aaa', 'bb', 'c']
  a.min == 'aaa' and
    a.min {|i1,i2| i1.length <=> i2.length} == 'c'
end

