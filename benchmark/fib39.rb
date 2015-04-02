# Fib 39

def fib n
  return n if n < 2
  fib(n-2) + fib(n-1)
end

printf fib(39)
