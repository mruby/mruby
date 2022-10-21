# mruby-set   [![Build Status](https://travis-ci.org/yui-knk/mruby-set.png?branch=master)](https://travis-ci.org/yui-knk/mruby-set)

Set class

## install by mrbgems 

- add conf.gem line to `build_config.rb` 

```ruby
MRuby::Build.new do |conf|

  # ... (snip) ...

  conf.gem :git => 'https://github.com/yui-knk/mruby-set.git'
end
```

## example 

```ruby
set1 = Set.new([1,2])
set2 = Set[1,2,3]
set3 = Set[4]

set1 + set3
#=> #<Set: {1, 2, 4}>
set2 - set1
#=> #<Set: {3}>
set2 & set1
#=> #<Set: {1, 2}>
set1 ^ set2
#=> #<Set: {3}>
```

## limitation

These methods are not implemented yet.

+ freeze
+ taint
+ untaint
+ to_set
+ divide(Set#divide with 2 arity block is not implemented.)

## License
under the MIT License:
- see LICENSE file
