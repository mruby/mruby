# Sleep Module for mruby

mruby sleep module

## Install by mrbgems

- add `conf.gem` line to your build configuration.

```ruby
MRuby::Build.new do |conf|

    # ... (snip) ...

    conf.gem :core => 'mruby-sleep'
end
```

## Example

```ruby
sleep(10)
usleep(10000)
```

# License

under the MIT License:

- <https://opensource.org/license/mit>
