# mruby-proc-ext

This mrbgem extends the functionality of the `Proc` class in mruby, providing additional methods for common operations and introspection.

## Features

This gem adds the following methods to the `Proc` class:

- **`===`**: Alias for `Proc#call`. Allows procs to be used in `case` statements.

  ```ruby
  upcase_proc = ->(s) { s.upcase }
  case "hello"
  when upcase_proc
    # this won't match directly, but demonstrates usage if proc returned boolean
  end
  # More practically:
  is_even = ->(n) { n % 2 == 0 }
  puts "4 is even" if is_even === 4 # => true
  ```

- **`yield`**: Alias for `Proc#call`.

  ```ruby
  add_one = ->(x) { x + 1 }
  puts add_one.yield(5) # => 6
  ```

- **`to_proc`**: Returns `self`. Useful for methods expecting a proc.

  ```ruby
  p = Proc.new { |x| x * 2 }
  [1, 2, 3].map(&p.to_proc) # => [2, 4, 6]
  ```

- **`curry(arity=self.arity)`**: Returns a curried proc.

  ```ruby
  adder = ->(a, b, c) { a + b + c }
  curried_adder = adder.curry
  add5 = curried_adder.call(2, 3)
  puts add5.call(4) # => 9

  mul = ->(x,y) { x * y }
  mul_by_5 = mul.curry.call(5)
  puts mul_by_5.call(3) # => 15
  ```

- **`<< (composition)`**: Returns a new proc that represents the composition of two procs (g << f is g(f(x))).

  ```ruby
  add_one = ->(x) { x + 1 }
  double_it = ->(x) { x * 2 }
  composed_proc = double_it << add_one # double_it(add_one(x))
  puts composed_proc.call(5) # => double_it(6) => 12
  ```

- **`>> (composition)`**: Returns a new proc that represents the composition of two procs (f >> g is g(f(x))).

  ```ruby
  add_one = ->(x) { x + 1 }
  double_it = ->(x) { x * 2 }
  composed_proc = add_one >> double_it # double_it(add_one(x))
  puts composed_proc.call(5) # => double_it(6) => 12
  ```

- **`lambda?`**: Returns `true` if the proc is a lambda, `false` otherwise.

  ```ruby
  my_lambda = -> {}
  my_proc = Proc.new {}
  puts my_lambda.lambda? # => true
  puts my_proc.lambda?  # => false
  ```

- **`source_location`**: Returns an array containing the source filename and line number where the proc was defined, or `nil` if this information is not available.

  ```ruby
  # Assuming this code is in "test.rb" at line 5
  my_proc = Proc.new {}
  p my_proc.source_location # => ["test.rb", 5] (if debug info enabled)
  ```

- **`inspect`**: Returns a string containing a human-readable representation of the proc, including its source location (if available) and whether it's a lambda.

  ```ruby
  my_lambda = ->(x) { x * 2 }
  # Might output: #<Proc:0x... test.rb:1 (lambda)>
  puts my_lambda.inspect
  ```

- **`parameters`**: Returns an array of arrays, describing the parameters accepted by the proc. Each inner array contains the parameter type (`:req`, `:opt`, `:rest`, `:key`, `:keyrest`, `:block`) and its name.

  ```ruby
  prc = ->(a, b=20, *rest, keyparam:, &blk) { }
  p prc.parameters
  # => [[:req, :a], [:opt, :b], [:rest, :rest], [:keyreq, :keyparam], [:block, :blk]]
  ```

This gem also adds:

- **`Kernel#proc`**: Creates a new proc from a block. This is similar to `Proc.new` but ensures the created proc is not a lambda.

  ```ruby
  p1 = proc { |a| a }
  puts p1.lambda? # => false
  ```
