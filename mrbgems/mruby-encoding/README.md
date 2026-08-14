# mruby-encoding

This mrbgem provides a lightweight, "poorman's" encoding functionality for mruby. It is designed to offer basic encoding support, primarily focused on UTF-8 and ASCII-8BIT.

## Summary

- **License:** MIT
- **Author:** mruby developers
- **Supported Encodings:**
  - `Encoding::ASCII_8BIT` (aliased as `Encoding::BINARY`)
  - `Encoding::UTF_8`, only in a build that defines `MRB_UTF8_STRING`

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
    Without `MRB_UTF8_STRING`, `"UTF-8"` is such a name.

### `Integer` Method

- `integer.chr(encoding_name = Encoding::BINARY) -> String`
  - Returns a single-character string represented by the integer.
  - If `encoding_name` is "UTF-8", the integer is treated as a Unicode codepoint.
  - If `encoding_name` is "ASCII-8BIT" or "BINARY" (the default), the integer is treated as a byte value (0-255).
  - Raises a `RangeError` if the integer is out of the valid range for the specified encoding.
  - Raises an `ArgumentError` for unknown encoding names, which without
    `MRB_UTF8_STRING` includes `"UTF-8"`.

## Builds without `MRB_UTF8_STRING`

This gem does not define `MRB_UTF8_STRING` for the build; a build that wants
UTF-8 defines it itself. Without it mruby reads a string as bytes, so there is
no UTF-8 for this gem to name and UTF-8 becomes an encoding the build has no
entry for:

- `Encoding::UTF_8` is not defined, so naming it raises `NameError`.
- `String#encoding` answers `Encoding::BINARY` for every string.
- `String#force_encoding("UTF-8")` raises
  `ArgumentError: unknown encoding name - UTF-8`, as any other unknown name
  does.
- `Integer#chr("UTF-8")` raises the same `ArgumentError`.
- `String#valid_encoding?` answers true for every string, because every sequence
  of bytes is valid where the string is read as bytes.

This is what CRuby does with a name it has no encoding for. To tell the two
builds apart, compare `__ENCODING__` against `"UTF-8"`.

## Usage Example

```ruby
# main.rb
if __ENCODING__ == "UTF-8"
  s = "helloあ"
  puts s.encoding        #=> UTF-8
  puts s.valid_encoding? #=> true

  # the bytes are not touched, only the way they are read
  bytes = "\xE3\x81\x82"      # UTF-8 bytes for "あ"
  puts bytes.encoding         #=> UTF-8
  puts bytes.length           #=> 1
  puts bytes.b.length         #=> 3

  broken = "\xff\xfe".force_encoding("UTF-8")
  puts broken.valid_encoding? #=> false

  s3 = "world"
  s3.force_encoding("BINARY")
  puts s3.encoding            #=> ASCII-8BIT
  puts s3.valid_encoding?     #=> true

  puts 12354.chr("UTF-8")     #=> "あ"
  # 0x110000.chr("UTF-8")     #=> RangeError
else
  s = "hello"
  puts s.encoding             #=> ASCII-8BIT

  s.force_encoding("BINARY")  # a name this build has
  # s.force_encoding("UTF-8") #=> ArgumentError: unknown encoding name - UTF-8
  # Encoding::UTF_8           #=> NameError: uninitialized constant Encoding::UTF_8
  # 12354.chr("UTF-8")        #=> ArgumentError: unknown encoding name - UTF-8
end

# Integer#chr reads a byte value whatever the build
puts 65.chr           #=> "A"
puts 65.chr("BINARY") #=> "A"
```
