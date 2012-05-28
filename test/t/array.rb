##
# Array ISO Test

assert('Array', '15.2.12') do
  Array.class == Class
end

assert('Array.[]', '15.2.12.4.1') do
  Array.[](1,2,3) == [1, 2, 3]
end

assert('Array#*', '15.2.12.5.1') do
  [1].*(3) == [1, 1, 1]
end

assert('Array#+', '15.2.12.5.2') do
  [1].+([1]) == [1, 1]
end

assert('Array#<<', '15.2.12.5.3') do
  [1].<<(1) == [1, 1]
end

assert('Array#[]', '15.2.12.5.4') do
  e2 = nil
  e3 = nil
  a = Array.new
  begin
    # this will cause an exception due to the wrong arguments
    a.[]()
  rescue => e1
    e2 = e1
  end
  begin
    # this will cause an exception due to the wrong arguments
    a.[](1,2,3)
  rescue => e1
    e3 = e1
  end

  [1,2,3].[](1) == 2 and
    e2.class == ArgumentError and
    e3.class == ArgumentError
end

assert('Array#[]=', '15.2.12.5.5') do
  e2 = nil
  e3 = nil
  a = Array.new
  begin
    # this will cause an exception due to the wrong arguments
    a.[]=()
  rescue => e1
    e2 = e1
  end
  begin
    # this will cause an exception due to the wrong arguments
    a.[]=(1,2,3,4)
  rescue => e1
    e3 = e1
  end

  [1,2,3].[]=(1,4) == [1, 4, 3] and
    e2.class == ArgumentError and
    e3.class == ArgumentError
end

assert('Array#clear', '15.2.12.5.6') do
  a = [1]
  a.clear
  a == []
end

assert('Array#collect!', '15.2.12.5.7') do
  a = [1,2,3]
  a.collect! { |i| i + i }
  a == [2,4,6]
end

assert('Array#concat', '15.2.12.5.8') do
  a = [1,2]
  b = [3,4]
  a.concat(b) == [1,2,3,4]
end

assert('Array#delete_at', '15.2.12.5.9') do
  a = [1,2,3]
  a.delete_at(1)
  a == [1,3]
end

assert('Array#each', '15.2.12.5.10') do
  a = [1,2,3]
  b = 0
  a.each {|i| b += i}
  b == 6
end

assert('Array#each_index', '15.2.12.5.11') do
  a = [1]
  b = nil
  a.each_index {|i| b = i}
  b == 0
end

assert('Array#empty?', '15.2.12.5.12') do
  a = []
  b = [b]
  a.empty? and not b.empty?
end

assert('Array#first', '15.2.12.5.13') do
  a = []
  b = [1,2,3]

  a.first == nil and b.first == 1
end

assert('Array#index', '15.2.12.5.14') do
  a = [1,2,3]

  a.index(2) == 1
end

assert('Array#initialize', '15.2.12.5.15') do
  a = [].initialize(1)
  b = [].initialize(2)
  c = [].initialize(2, 1)
  d = [].initialize(2) {|i| i}

  a == [nil] and b == [nil,nil] and c == [1,1] and d == [0,1]
end

assert('Array#initialize_copy', '15.2.12.5.16') do
  a = [1,2,3]
  b = [].initialize_copy(a)

  b == [1,2,3]
end

assert('Array#join', '15.2.12.5.17') do
  a = [1,2,3].join
  b = [1,2,3].join(',')

  a == '123' and b == '1,2,3'
end

assert('Array#last', '15.2.12.5.18') do
  a = [1,2,3]

  a.last == 3 and [].last == nil
end

assert('Array#length', '15.2.12.5.19') do
  a = [1,2,3]

  a.length == 3
end

assert('Array#map!', '15.2.12.5.20') do
  a = [1,2,3]
  a.map! { |i| i + i }
  a == [2,4,6]
end

assert('Array#pop', '15.2.12.5.21') do
  a = [1,2,3]
  b = a.pop

  [].pop == nil and a == [1,2] and b = 3
end

assert('Array#push', '15.2.12.5.22') do
  a = [1,2,3]
  b = a.push(4)

  a == [1,2,3,4] and b = [1,2,3,4]
end

assert('Array#replace', '15.2.12.5.23') do
  a = [1,2,3]
  b = [].replace(a)

  b == [1,2,3]
end

assert('Array#reverse', '15.2.12.5.24') do
  a = [1,2,3]
  b = a.reverse

  a == [1,2,3] and b == [3,2,1]
end

assert('Array#reverse!', '15.2.12.5.25') do
  a = [1,2,3]
  b = a.reverse!

  a == [3,2,1] and b == [3,2,1]
end

assert('Array#rindex', '15.2.12.5.26') do
  a = [1,2,3]

  a.rindex(2) == 1
end

assert('Array#shift', '15.2.12.5.27') do
  a = [1,2,3]
  b = a.shift

  [].shift == nil and a == [2,3] and b == 1
end

assert('Array#size', '15.2.12.5.28') do
  a = [1,2,3]

  a.size == 3
end

assert('Array#slice', '15.2.12.5.29') do
  [1,2,3].[](1) == 2
end

assert('Array#unshift', '15.2.12.5.30') do
  a = [2,3]
  b = a.unshift(1)

  a == [1,2,3] and b == [1,2,3]
end

# Not ISO specified
