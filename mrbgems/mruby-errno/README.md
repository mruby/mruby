# mruby-errno

Errno module for mruby

This mrbgem provides the `Errno` module, which allows mruby programs to access system error numbers and their corresponding error messages. This is particularly useful for handling errors returned by underlying system calls.

## Functionality

The `mruby-errno` gem defines the `Errno` module and the `SystemCallError` class.

### Accessing Error Constants

The `Errno` module provides constants for various system errors. Each error constant corresponds to a specific system error number.

Example:

```ruby
p Errno::EPERM::Errno  # Output: 1 (or the system-specific value for EPERM)
p Errno::EPERM.new.message # Output: "Operation not permitted"
```

### Raising and Rescuing System Call Errors

The `SystemCallError` class is the base class for system call errors. You can raise instances of `SystemCallError` or its subclasses (like `Errno::EPERM`) and rescue them in your code.

Example:

```ruby
begin
  raise Errno::EACCES, "my_method"
rescue SystemCallError => e
  p e.class           # Output: Errno::EACCES
  p e.errno           # Output: 13 (or the system-specific value for EACCES)
  p e.message         # Output: "Permission denied - my_method"
  p e.to_s            # Output: "Permission denied - my_method"
end

# You can also rescue specific Errno constants
begin
  # Simulate a system call that might fail
  # For example, trying to open a file without permissions
  raise Errno::ENOENT, "missing_file.txt"
rescue Errno::ENOENT => e
  puts "Caught an ENOENT error: #{e.message}"
  # Output: Caught an ENOENT error: No such file or directory - missing_file.txt
end
```

### Errno::NOERROR

The `Errno::NOERROR` constant represents the absence of an error, typically with a value of 0.

Example:

```ruby
p Errno::NOERROR::Errno # Output: 0
```

## Defining Error Types

New error types and their corresponding numbers are defined in the `known_errors.def` file. The `gen.rb` script processes this file to generate the necessary C and Ruby code for the `Errno` module. This allows `mruby-errno` to be easily extended with new system error definitions.

## License

Copyright (c) 2013 Internet Initiative Japan Inc.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
