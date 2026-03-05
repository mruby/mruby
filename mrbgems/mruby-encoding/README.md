# mruby-encoding

This mrbgem provides a lightweight, "poorman's" encoding functionality for mruby. It is designed to offer basic encoding support, primarily focused on UTF-8 and ASCII-8BIT.

## Summary

- **License:** MIT
- **Author:** mruby developers
- **Supported Encodings:**
  - `Encoding::UTF_8`
  - `Encoding::ASCII_8BIT` (aliased as `Encoding::BINARY`)

## Functionality

This gem introduces an `Encoding` module and extends the `String` and `Integer` classes with encoding-related methods.

### `Encoding` Module

A module (not a class, unlike standard Ruby) that holds encoding constants.

- `Encoding::UTF_8`: Represents the UTF-8 encoding.
- `Encoding::ASCII_8BIT`: Represents the ASCII-8BIT encoding.
- `Encoding::BINARY`: An alias for `Encoding::ASCII_8BIT`.

### `String` Methods

- `string.valid_encoding? -> true or false`
  - Returns `true` if the string is correctly encoded (particularly useful for UTF-8 strings). For `ASCII-8BIT` strings, it generally returns `true`.
- `string.encoding -> EncodingConstant`
  - Returns the encoding of the string. This will be `Encoding::UTF_8` or `Encoding::BINARY`.
- `string.force_encoding(encoding_name) -> string`
  - Changes the string's reported encoding to the specified `encoding_name` (e.g., "UTF-8", "ASCII-8BIT", "BINARY").
  - The actual byte sequence of the string is not changed.
  - Raises an `ArgumentError` if an unsupported encoding name is provided.

### `Integer` Method

- `integer.chr(encoding_name = Encoding::BINARY) -> String`
  - Returns a single-character string represented by the integer.
  - If `encoding_name` is "UTF-8", the integer is treated as a Unicode codepoint.
  - If `encoding_name` is "ASCII-8BIT" or "BINARY" (the default), the integer is treated as a byte value (0-255).
  - Raises a `RangeError` if the integer is out of the valid range for the specified encoding.
  - Raises an `ArgumentError` for unknown encoding names.

## Usage Example

```ruby
# main.rb
if __ENCODING__ == "UTF-8"
  s = "helloあ"
  puts s.encoding  #=> Encoding::UTF_8
  puts s.valid_encoding? #=> true

  s2 = "\xff".force_encoding("UTF-8")
  puts s2.valid_encoding? #=> false

  s3 = "world"
  s3.force_encoding("BINARY")
  puts s3.encoding #=> Encoding::BINARY
  puts s3.valid_encoding? #=> true (ASCII-8BIT strings are generally considered valid)

  puts 65.chr #=> "A" (defaults to ASCII-8BIT)
  puts 230.chr("UTF-8") #=> "æ" (if U+00E6 is æ)
  # For mruby, this might be different based on actual UTF-8 char mapping
  # For example, 12354.chr("UTF-8") might be "あ"
else
  s = "hello"
  puts s.encoding #=> Encoding::BINARY (or ASCII-8BIT)

  # Attempting to force to UTF-8 in a non-UTF-8 mruby build might be limited
  # or behave as ASCII-8BIT depending on mruby's core string handling.
end

# Force encoding
my_string = "\xE3\x81\x82" # UTF-8 bytes for "あ"
puts my_string.encoding # Might be BINARY by default if not created as UTF-8 literal

my_string.force_encoding("UTF-8")
puts my_string.encoding #=> Encoding::UTF_8
puts my_string #=> あ

invalid_utf8 = "\xff\xfe"
invalid_utf8.force_encoding("UTF-8")
puts invalid_utf8.valid_encoding? #=> false

# Integer#chr
puts 65.chr # => "A"
puts 65.chr("BINARY") # => "A"

# When mruby is compiled with MRB_UTF8_STRING
if Object.const_defined?(:MRB_UTF8_STRING)
  puts 12354.chr("UTF-8") # => "あ"
  # puts 0x110000.chr("UTF-8") #=> RangeError
end

```
