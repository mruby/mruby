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
p Set.hi
#=> "hi!!"
t = Set.new "hello"
p t.hello
#=> "hello"
p t.bye
#=> "hello bye"
```

## License
under the MIT License:
- see LICENSE file
