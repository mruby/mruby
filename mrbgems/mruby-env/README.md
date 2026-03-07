# mruby-env

ENV object for environment variable access.

This gem is a built-in replacement for [iij/mruby-env](https://github.com/iij/mruby-env),
providing a superset of its API.

## Methods

`[]`, `[]=`, `assoc`, `clear`, `delete`, `each`, `each_key`,
`each_value`, `empty?`, `fetch`, `filter`/`select`, `freeze`,
`has_key?`/`include?`/`key?`/`member?`, `has_value?`/`value?`,
`inspect`, `key`, `keys`, `length`/`size`, `merge!`/`update`,
`rassoc`, `reject`, `replace`, `slice`, `store`, `to_a`, `to_h`,
`to_s`, `values`

ENV includes `Enumerable` via `mruby-enumerator`.

## Example

```ruby
ENV["MY_VAR"] = "hello"
ENV["MY_VAR"]               #=> "hello"
ENV.delete("MY_VAR")        #=> "hello"

ENV.keys                    #=> ["PATH", "HOME", ...]
ENV.each { |k, v| puts "#{k}=#{v}" }
ENV.select { |k, v| k.start_with?("RUBY") }
```

## License

MIT License - see the mruby license.
