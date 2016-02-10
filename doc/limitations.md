# Limitations

The philosophy of mruby is to be a lightweight implementation of
the Ruby ISO standard. These two objectives are partially contradicting.
Ruby is an expressive language with complex implementation details which
are difficult to implement in a lightweight manner. To cope with this,
limitations to the "Ruby Compatibility" are defined.

This document is collecting these limitations.

## Integrity

This document does not contain a complete list of limitations.
Please help to improve it by submitting your findings.

## ```Array``` passed to ```puts```

Passing an Array to ```puts``` results in different output.

```ruby
puts [1,2,3]
```

#### Ruby [ruby 2.0.0p645 (2015-04-13 revision 50299)]

```
1
2
3
```

#### mruby [1.2.0 (2015-11-17)]

```
[1, 2, 3]
```

## ```Kernel.raise``` in rescue clause

```Kernel.raise``` without arguments does not raise the current exception within
a rescue clause.

```ruby
begin
  1 / 0
rescue
  raise
end
```

#### Ruby [ruby 2.0.0p645 (2015-04-13 revision 50299)]

```ZeroDivisionError``` is raised.

#### mruby [1.2.0 (2015-11-17)]

No exception is raised.
