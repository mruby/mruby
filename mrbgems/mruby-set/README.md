# mruby-set

`mruby-set` provides a `Set` class for mruby, offering a collection of unordered, unique elements. It's useful when you need to store a group of items and quickly check for membership, or perform set operations like union, intersection, and difference.

## Installation

To use `mruby-set` in your mruby project, add the following to your `build_config.rb`:

```ruby
conf.gem "#{MRUBY_ROOT}/mrbgems/mruby-set"
```

Or, if you have it as a separate gem:

```ruby
conf.gem :github => "mruby/mruby-set"
```

## Usage and Examples

### Creating a Set

You can create a set from an array or by using the `Set.[]` shorthand:

```ruby
require 'set' # Not strictly necessary in mruby if compiled in

set1 = Set.new([1, 2, 3])
#=> Set[1, 2, 3]

set2 = Set[3, 4, 5]
#=> Set[3, 4, 5]
```

### Adding and Deleting Elements

```ruby
s = Set.new
s.add(10)    #=> Set[10]
s << 20      #=> Set[10, 20]
s.add?(30)   #=> Set[10, 20, 30]
s.add?(20)   #=> nil (20 is already in the set)

s.delete(10) #=> Set[20, 30]
s.delete?(5) #=> nil (5 was not in the set)
s.delete?(20) #=> Set[30]
```

### Set Operations

`mruby-set` supports common set operations:

**Union (`|`, `+`, `union`):** Returns a new set containing all elements from both sets.

```ruby
set_a = Set[1, 2, 3]
set_b = Set[3, 4, 5]

set_a | set_b  #=> Set[1, 2, 3, 4, 5]
set_a + set_b  #=> Set[1, 2, 3, 4, 5]
```

**Intersection (`&`, `intersection`):** Returns a new set containing elements common to both sets.

```ruby
set_a = Set[1, 2, 3]
set_b = Set[3, 4, 5]

set_a & set_b  #=> Set[3]
```

**Difference (`-`, `difference`):** Returns a new set containing elements from the first set that are not in the second set.

```ruby
set_a = Set[1, 2, 3]
set_b = Set[3, 4, 5]

set_a - set_b  #=> Set[1, 2]
```

**Exclusive OR (`^`):** Returns a new set containing elements that are in one or the other of the sets, but not in both.

```ruby
set_a = Set[1, 2, 3]
set_b = Set[3, 4, 5]

set_a ^ set_b  #=> Set[1, 2, 4, 5]
```

### Querying the Set

**Checking for inclusion (`include?`, `member?`, `===`):**

```ruby
s = Set["apple", "banana", "cherry"]
s.include?("banana")  #=> true
s.member?("grape")    #=> false
```

**Checking size (`size`, `length`):**

```ruby
s = Set[10, 20, 30]
s.size #=> 3
```

**Checking if empty (`empty?`):**

```ruby
Set.new.empty? #=> true
Set[1].empty?  #=> false
```

**Subset and Superset (`subset?`, `superset?`, `<`, `<=`, `>`, `>=`):**

```ruby
set_main = Set[1, 2, 3, 4]
sub = Set[2, 3]
super_set = Set[1, 2, 3, 4, 5]

sub.subset?(set_main)         #=> true
set_main.superset?(sub)       #=> true
set_main < super_set          #=> true (proper subset)
super_set > set_main          #=> true (proper superset)
Set[1,2].proper_subset?(Set[1,2,3]) #=> true
Set[1,2,3].proper_superset?(Set[1,2]) #=> true
```

**Disjoint (`disjoint?`):** Returns `true` if the set has no elements in common with the given set.

```ruby
Set[1, 2].disjoint?(Set[3, 4]) #=> true
Set[1, 2].disjoint?(Set[2, 3]) #=> false
```

**Intersect (`intersect?`):** Returns `true` if the set has any elements in common with the given set.

```ruby
Set[1, 2].intersect?(Set[2, 3]) #=> true
Set[1, 2].intersect?(Set[3, 4]) #=> false
```

### Other Useful Methods

**Convert to Array (`to_a`):**

```ruby
s = Set["a", "b", "c"]
s.to_a #=> ["a", "b", "c"] (order may vary)
```

**Iterating (`each`):**

```ruby
s = Set[1, 2, 3]
s.each { |x| puts x * 10 }
# Output:
# 10
# 20
# 30
```

**Map/Collect (`map!`, `collect!`):** Modifies the set by applying the block to each element.

```ruby
s = Set[1, 2, 3]
s.map! { |x| x * x } #=> Set[1, 4, 9]
```

**Select/Filter (`select!`, `filter!`):** Keeps elements for which the block returns true.

```ruby
s = Set[1, 2, 3, 4, 5]
s.select! { |x| x.even? } #=> Set[2, 4]
```

**Reject (`reject!`):** Deletes elements for which the block returns true.

```ruby
s = Set[1, 2, 3, 4, 5]
s.reject! { |x| x.odd? } #=> Set[2, 4]
```

**Clear (`clear`):** Removes all elements from the set.

```ruby
s = Set[1, 2, 3]
s.clear #=> Set[]
```

**Replace (`replace`):** Replaces the contents of the set with the contents of the given enumerable.

```ruby
s = Set[1, 2, 3]
s.replace([4, 5]) #=> Set[4, 5]
```

**Flatten (`flatten`, `flatten!`):** Returns a new set that is a copy of the set, flattening any nested sets. `flatten!` modifies the set in place.

```ruby
s = Set[1, Set[2, 3], 4]
s.flatten #=> Set[1, 2, 3, 4]
```

## Method Overview

Here's a list of commonly used methods available in `mruby-set`:

- `Set.[](*ary)`
- `initialize(enum = nil, &block)`
- `size`, `length`
- `empty?`
- `clear`
- `replace(enum)`
- `to_a`
- `include?(o)`, `member?(o)`, `===`
- `superset?(set)`, `>=`
- `proper_superset?(set)`, `>`
- `subset?(set)`, `<=`
- `proper_subset?(set)`, `<`
- `intersect?(set)`
- `disjoint?(set)`
- `each(&block)`
- `add(o)`, `<<(o)`
- `add?(o)`
- `delete(o)`
- `delete?(o)`
- `delete_if { |o| ... }`
- `keep_if { |o| ... }`
- `collect! { |o| ... }`, `map! { |o| ... }`
- `reject! { |o| ... }`
- `select! { |o| ... }`, `filter! { |o| ... }`
- `merge(enum)`
- `subtract(enum)`
- `|(enum)`, `+(enum)`, `union(enum)`
- `-(enum)`, `difference(enum)`
- `&(enum)`, `intersection(enum)`
- `^(enum)`
- `==(other)`
- `hash`
- `eql?(o)`
- `classify { |o| ... }`
- `divide(&func)`
- `join(separator = nil)`
- `inspect`, `to_s`
- `flatten`, `flatten!`

## Limitations

These methods are not implemented yet:

- freeze
- to_set
- divide(Set#divide with 2 arity block is not implemented.)

## License

Under the MIT License:

- see [LICENSE](LICENSE) file
