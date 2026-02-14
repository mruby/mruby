
def fib n
  return n if n < 2
  fib(n-2) + fib(n-1)
end

puts 1.upto(15).collect {|i| fib(i)}.join(',')
