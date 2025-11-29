# mruby-range-ext

This gem extends the functionality of Ruby's `Range` class in mruby. It provides several new methods to make working with ranges more convenient and powerful.

## Methods

### `first` / `first(n)`

Returns the first object in the range, or an array of the first `n` elements.

**Example:**

```ruby
(10..20).first     #=> 10
(10..20).first(3)  #=> [10, 11, 12]
```

### `last` / `last(n)`

Returns the last object in the range, or an array of the last `n` elements. Note that with no arguments `last` will return the object that defines the end of the range even if `exclude_end?` is true.

**Example:**

```ruby
(10..20).last      #=> 20
(10...20).last     #=> 20
(10..20).last(3)   #=> [18, 19, 20]
(10...20).last(3)  #=> [17, 18, 19]
```

### `max`

Returns the maximum value in the range. Returns `nil` if the range is empty or excludes its end and the end is not an Integer. For non-numeric ranges or when a block is given, it delegates to `Enumerable#max`.

**Example:**

```ruby
(10..20).max      #=> 20
(10...20).max     #=> 19
('a'..'z').max    #=> "z"
```

### `min`

Returns the minimum value in the range. For non-numeric ranges or when a block is given, it delegates to `Enumerable#min`.

**Example:**

```ruby
(10..20).min      #=> 10
('a'..'z').min    #=> "a"
```

### `overlap?(other_range)`

Returns `true` if `self` and `other_range` have at least one element in common.

**Example:**

```ruby
(1..5).overlap?(4..6) #=> true
(1..5).overlap?(7..9) #=> false
```

### `cover?(obj_or_range)`

Returns `true` if the given object or all elements of the given range are within `self`.

**Example:**

```ruby
("a".."z").cover?("c")    #=> true
("a".."z").cover?("5")    #=> false
("a".."z").cover?("cc")   #=> true # Note: This behavior might be surprising for strings.
(1..5).cover?(2..4)      #=> true
(1..5).cover?(2..7)      #=> false
```

### `size`

Returns the number of elements in the range. Both the begin and the end of the Range must be Numeric, otherwise `nil` is returned. For endless ranges with an Integer beginning, it returns `Infinity`.

**Example:**

```ruby
(10..20).size    #=> 11
('a'..'z').size  #=> nil
(1..).size       #=> Infinity
```

## License

This gem is released under the MIT License.
