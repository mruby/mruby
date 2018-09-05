# Sleep Module for mruby
mruby sleep module

## install by mrbgems
 - add conf.gem line to `build_config.rb`
```ruby
MRuby::Build.new do |conf|

    # ... (snip) ...

    conf.gem :git => 'https://github.com/matsumoto-r/mruby-sleep.git'
end
```

## example

```ruby
Sleep::sleep(10)
Sleep::usleep(10000)
```

# License
under the MIT License:

* http://www.opensource.org/licenses/mit-license.php


