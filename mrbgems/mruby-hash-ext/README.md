# mruby-hash-ext

This mrbgem extends the core `Hash` class in mruby, providing a collection of additional methods to enhance its functionality. These extensions offer more ways to manipulate and interact with hashes, drawing inspiration from common Ruby hash methods.

## How to Use

To incorporate `mruby-hash-ext` into your mruby project, add it to your `build_config.rb` file. For example:

```ruby
MRuby::Build.new do |conf|
  # ... other configurations ...
  conf.gem :github => 'mruby/mruby-hash-ext'
  # or if you have it locally:
  # conf.gem "#{root}/mrbgems/mruby-hash-ext"
end
```

Then, rebuild your mruby project. The extended hash methods will then be available for use.

## Implemented Methods

This gem implements the following methods for the `Hash` class:

- `values_at(*keys)`: Returns an array containing the values associated with the given keys.
- `slice(*keys)`: Returns a hash containing only the given keys and their values.
- `except(*keys)`: Returns a hash excluding the given keys and their values.
- `Hash.[](*object)`: Creates a new hash populated with the given objects.
- `#merge!(other_hash..)`, `#update(other_hash..)`: Adds the contents of `other_hash` to `hsh`.
- `#compact!`: Removes all nil values from the hash.
- `#compact`: Returns a new hash with the nil values/key pairs removed.
- `#fetch(key [, default])`, `#fetch(key) {| key | block }`: Returns a value from the hash for the given key.
- `#delete_if {| key, value | block }`: Deletes every key-value pair from `hsh` for which `block` evaluates to `true`.
- `#flatten`: Returns a new array that is a one-dimensional flattening of this hash.
- `#invert`: Returns a new hash created by using `hsh`'s values as keys, and the keys as values.
- `#keep_if {| key, value | block }`: Deletes every key-value pair from `hsh` for which `block` evaluates to false.
- `#key(value)`: Returns the key of an occurrence of a given value.
- `#to_h`: Returns `self`. If called on a subclass of Hash, converts the receiver to a Hash object.
- `#< other_hash`: Returns `true` if `hsh` is a subset of `other_hash`.
- `#<= other_hash`: Returns `true` if `hsh` is a subset of `other_hash` or equal to `other_hash`.
- `#> other_hash`: Returns `true` if `other_hash` is a subset of `hsh`.
- `#>= other_hash`: Returns `true` if `other_hash` is a subset of `hsh` or equal to `hsh`.
- `#dig(key, ...)`: Extracts the nested value specified by the sequence of keys.
- `#transform_keys {|key| block }`: Returns a new hash with keys transformed by the block.
- `#transform_keys! {|key| block }`: Modifies the hash by transforming its keys using the block.
- `#transform_values {|value| block }`: Returns a new hash with values transformed by the block.
- `#transform_values! {|value| block }`: Modifies the hash by transforming its values using the block.
- `#to_proc`: Returns a proc that maps a key to its value in the hash.
- `#fetch_values(key, ...)`: Returns an array of values for the given keys, raising KeyError if any are not found.
- `#filter {| key, value | block }` (Alias for `select`): Returns a new hash containing entries for which the block returns true.
- `#filter! {| key, value | block }` (Alias for `select!`): Modifies the hash, keeping only entries for which the block returns true.

### `values_at(*keys) -> array`

Returns an array containing the values associated with the given keys. If a key is not found, `nil` is returned for that key's position in the array.

```ruby
h = { "cat" => "feline", "dog" => "canine", "cow" => "bovine" }
h.values_at("cow", "cat")  #=> ["bovine", "feline"]
h.values_at("dog", "mouse") #=> ["canine", nil]
h.values_at()              #=> []
```

### `slice(*keys) -> new_hash`

Returns a new hash containing only the given keys and their associated values from the original hash. If a key is not found in the original hash, it's ignored.

```ruby
h = { a: 100, b: 200, c: 300 }
h.slice(:a)           #=> {a: 100}
h.slice(:b, :c, :d)   #=> {b: 200, c: 300} (ignores :d as it's not in h)
h.slice()             #=> {}
```

### `except(*keys) -> new_hash`

Returns a new hash containing all key-value pairs from the original hash except for those specified by the given keys. If a key is not found in the original hash, it's ignored.

```ruby
h = { a: 100, b: 200, c: 300 }
h.except(:a)          #=> {b: 200, c: 300}
h.except(:b, :c, :d)  #=> {a: 100} (ignores :d as it's not in h)
h.except()            #=> {a: 100, b: 200, c: 300}
```

### `Hash.[](*object)`

Creates a new hash populated with the given objects.

- **`Hash[key, value, ...]`**: Creates a new hash with key-value pairs.
- **`Hash[[ [key, value], ... ]]`**: Creates a new hash from an array of key-value pairs.
- **`Hash[object]`**: Creates a new hash from an object convertible to a hash.

```ruby
h1 = Hash["a", 100, "b", 200] #=> {"a"=>100, "b"=>200}
h2 = Hash[[ ["a", 100], ["b", 200] ]] #=> {"a"=>100, "b"=>200}
h3 = Hash["a" => 100, "b" => 200] #=> {"a"=>100, "b"=>200}
```

### `#merge!(other_hash..) -> hsh`

### `#merge!(other_hash..){|key, oldval, newval| block} -> hsh`

(Alias: `#update`)

Adds the contents of `other_hash` to `hsh`. If no block is specified, entries with duplicate keys are overwritten with the values from `other_hash`. Otherwise, the value of each duplicate key is determined by calling the block with the key, its value in `hsh`, and its value in `other_hash`.

```ruby
h1 = { "a" => 100, "b" => 200 }
h2 = { "b" => 254, "c" => 300 }
h1.merge!(h2)   #=> {"a"=>100, "b"=>254, "c"=>300}

h1 = { "a" => 100, "b" => 200 }
h2 = { "b" => 254, "c" => 300 }
h1.merge!(h2) { |key, v1, v2| v1 }
                #=> {"a"=>100, "b"=>200, "c"=>300}
```

### `#compact! -> hsh`

Removes all nil values from the hash. Returns the hash. Returns `nil` if the hash does not contain nil values.

```ruby
h = { a: 1, b: false, c: nil }
h.compact!     #=> { a: 1, b: false }
```

### `#compact -> new_hsh`

Returns a new hash with the nil values/key pairs removed.

```ruby
h = { a: 1, b: false, c: nil }
h.compact     #=> { a: 1, b: false }
h             #=> { a: 1, b: false, c: nil }
```

### `#fetch(key [, default] ) -> obj`

### `#fetch(key) {| key | block } -> obj`

Returns a value from the hash for the given key.
If the key can't be found, there are several options:

- With no other arguments, it will raise a `KeyError` exception.
- If `default` is given, then that will be returned.
- If the optional code block is specified, then that will be run and its result returned.

```ruby
h = { "a" => 100, "b" => 200 }
h.fetch("a")                            #=> 100
h.fetch("z", "go fish")                 #=> "go fish"
h.fetch("z") { |el| "go fish, #{el}"}   #=> "go fish, z"

# h.fetch("z") # Raises KeyError: key not found: "z"
```

### `#delete_if {| key, value | block } -> hsh`

### `#delete_if -> an_enumerator`

Deletes every key-value pair from `hsh` for which `block` evaluates to `true`.
If no block is given, an enumerator is returned instead.

```ruby
h = { "a" => 100, "b" => 200, "c" => 300 }
h.delete_if {|key, value| key >= "b" }   #=> {"a"=>100}
```

### `#flatten -> an_array`

### `#flatten(level) -> an_array`

Returns a new array that is a one-dimensional flattening of this hash. That is, for every key or value that is an array, extract its elements into the new array. Unlike `Array#flatten`, this method does not flatten recursively by default. The optional `level` argument determines the level of recursion to flatten.

```ruby
a =  {1=> "one", 2 => [2,"two"], 3 => "three"}
a.flatten    # => [1, "one", 2, [2, "two"], 3, "three"]
a.flatten(2) # => [1, "one", 2, 2, "two", 3, "three"]
```

### `#invert -> new_hash`

Returns a new hash created by using `hsh`'s values as keys, and the keys as values. If a value appears more than once, the last key encountered will be used due to hash key uniqueness.

```ruby
h = { "n" => 100, "m" => 100, "y" => 300, "d" => 200, "a" => 0 }
h.invert   #=> {0=>"a", 100=>"m", 200=>"d", 300=>"y"}
```

### `#keep_if {| key, value | block } -> hsh`

### `#keep_if -> an_enumerator`

Deletes every key-value pair from `hsh` for which `block` evaluates to `false`.
If no block is given, an enumerator is returned instead.

```ruby
h = { "a" => 1, "b" => 2, "c" => 3, "d" => 4 }
h.keep_if {|key, value| value % 2 == 0 } #=> {"b"=>2, "d"=>4}
```

### `#key(value) -> key`

Returns the key of an occurrence of a given value. If the value is not found, returns `nil`.

```ruby
h = { "a" => 100, "b" => 200, "c" => 300, "d" => 300 }
h.key(200)   #=> "b"
h.key(300)   #=> "c" (returns the first key found for the value)
h.key(999)   #=> nil
```

### `#to_h -> hsh or new_hash`

Returns `self`. If called on a subclass of Hash, converts the receiver to a Hash object. For a Hash instance, it simply returns `self`.

```ruby
h = { "a" => 1, "b" => 2 }
h.to_h #=> {"a"=>1, "b"=>2}

class MyHash < Hash; end
my_h = MyHash["c" => 3, "d" => 4]
my_h.to_h #=> {"c"=>3, "d"=>4} (returns a Hash object, not MyHash)
```

### `#< other_hash -> true or false`

Returns `true` if `hsh` is a proper subset of `other_hash` (i.e., `other_hash` contains all key/value pairs of `hsh`, and `other_hash` has at least one additional key/value pair).

```ruby
h1 = {a:1, b:2}
h2 = {a:1, b:2, c:3}
h1 < h2    #=> true
h2 < h1    #=> false
h1 < h1    #=> false
```

### `#<= other_hash -> true or false`

Returns `true` if `hsh` is a subset of `other_hash` or is equal to `other_hash` (i.e., `other_hash` contains all key/value pairs of `hsh`).

```ruby
h1 = {a:1, b:2}
h2 = {a:1, b:2, c:3}
h1 <= h2   #=> true
h2 <= h1   #=> false
h1 <= h1   #=> true
```

### `#> other_hash -> true or false`

Returns `true` if `other_hash` is a proper subset of `hsh` (i.e., `hsh` contains all key/value pairs of `other_hash`, and `hsh` has at least one additional key/value pair).

```ruby
h1 = {a:1, b:2}
h2 = {a:1, b:2, c:3}
h1 > h2    #=> false
h2 > h1    #=> true
h1 > h1    #=> false
```

### `#>= other_hash -> true or false`

Returns `true` if `other_hash` is a subset of `hsh` or is equal to `hsh` (i.e., `hsh` contains all key/value pairs of `other_hash`).

```ruby
h1 = {a:1, b:2}
h2 = {a:1, b:2, c:3}
h1 >= h2   #=> false
h2 >= h1   #=> true
h1 >= h1   #=> true
```

### `#dig(key,...) -> object`

Extracts the nested value specified by the sequence of `key` objects by calling `dig` at each step. Returns `nil` if any intermediate step is `nil`.

```ruby
h = { a: { b: { c: 1 } } }
h.dig(:a, :b, :c)    #=> 1
h.dig(:a, :x, :c)    #=> nil

g = { a: [1, 2, {b: 3}] }
g.dig(:a, 2, :b)     #=> 3
g.dig(:a, 1, :b)     #=> nil (element at index 1 is 2, which does not respond to #dig)
```

### `#transform_keys {|key| block } -> new_hash`

### `#transform_keys -> an_enumerator`

Returns a new hash, with the keys computed from running the block once for each key in the hash, and the values unchanged.
If no block is given, an enumerator is returned instead.

```ruby
h = { a: 1, b: 2, c: 3 }
h.transform_keys {|k| k.to_s.upcase } #=> {"A"=>1, "B"=>2, "C"=>3}
```

### `#transform_keys! {|key| block } -> hsh`

### `#transform_keys! -> an_enumerator`

Invokes the given block once for each key in `hsh`, replacing it with the new key returned by the block, and then returns `hsh`.
If no block is given, an enumerator is returned instead.

```ruby
h = { a: 1, b: 2, c: 3 }
h.transform_keys! {|k| k.to_s.upcase } #=> {"A"=>1, "B"=>2, "C"=>3}
h #=> {"A"=>1, "B"=>2, "C"=>3}
```

### `#transform_values {|value| block } -> new_hash`

### `#transform_values -> an_enumerator`

Returns a new hash with the results of running the block once for every value. This method does not change the keys.
If no block is given, an enumerator is returned instead.

```ruby
h = { a: 1, b: 2, c: 3 }
h.transform_values {|v| v * v } #=> {a: 1, b: 4, c: 9}
```

### `#transform_values! {|value| block } -> hsh`

### `#transform_values! -> an_enumerator`

Invokes the given block once for each value in the hash, replacing it with the new value returned by the block, and then returns `hsh`.
If no block is given, an enumerator is returned instead.

```ruby
h = { a: 1, b: 2, c: 3 }
h.transform_values! {|v| v * v } #=> {a: 1, b: 4, c: 9}
h #=> {a: 1, b: 4, c: 9}
```

### `#to_proc -> a_proc`

Returns a `Proc` object that maps a key to its corresponding value in the hash. This allows a hash to be used as a block argument.

```ruby
h = {a: 1, b: 2}
p = h.to_proc
p.call(:a) #=> 1

['a', 'b', 'c'].map(&h) #=> [1, 2, nil] (uses h[x] for each element)
```

### `#fetch_values(key, ...) -> array`

### `#fetch_values(key, ...) { |key| block } -> array`

Returns an array containing the values associated with the given keys. Raises `KeyError` if any of the keys can't be found, unless a block is provided to compute a default value.

```ruby
h = { "cat" => "feline", "dog" => "canine", "cow" => "bovine" }

h.fetch_values("cow", "cat")                   #=> ["bovine", "feline"]
# h.fetch_values("cow", "bird")                  # raises KeyError: key not found: "bird"
h.fetch_values("cow", "bird") { |k| k.upcase } #=> ["bovine", "BIRD"]
```

### `#filter {| key, value | block } -> new_hsh`

### `#filter -> an_enumerator`

(Alias for `select`)
Returns a new hash consisting of entries for which the block returns a true value.
If no block is given, an enumerator is returned instead.

```ruby
h = { "a" => 100, "b" => 200, "c" => 300 }
h.filter {|key, value| key < "b"}  #=> {"a"=>100}
h.filter {|key, value| value < 200} #=> {"a"=>100}
```

### `#filter! {| key, value | block } -> hsh_or_nil`

### `#filter! -> an_enumerator`

(Alias for `select!`)
Equivalent to `Hash#keep_if`, but returns `nil` if no changes were made.
Deletes every key-value pair from `hsh` for which `block` evaluates to `false`.
If no block is given, an enumerator is returned instead.

```ruby
h = { "a" => 100, "b" => 200, "c" => 300 }
h.filter! {|key, value| key < "b"}  #=> {"a"=>100}
h                                   #=> {"a"=>100}
h.filter! {|key, value| key < "a"}  #=> nil (no changes)
```
