# mruby-pack

The `mruby-pack` mrbgem enhances mruby by providing `Array#pack` and `String#unpack` methods. These methods facilitate the conversion between Ruby data types (specifically arrays and strings) and binary data representations, offering a powerful way to handle binary data.

## Installation

Add the line below into your build configuration:

```ruby
conf.gem :core => 'mruby-pack'
```

There is no dependency on other mrbgems.

## `Array#pack`

The `Array#pack` method converts an array into a binary string. It takes a template string as an argument, which dictates how each element in the array should be converted and packed into the resulting binary string. The template string consists of directives, where each directive specifies the type and format of the data to be packed.

**Example:**

```ruby
arr = [65, 66, 67]
binary_string = arr.pack("ccc") # Packs three 8-bit signed integers
# binary_string will be "ABC"

arr2 = [72, 101, 108, 108, 111]
binary_string2 = arr2.pack("C*") # Packs all elements as 8-bit unsigned integers
# binary_string2 will be "Hello"

arr3 = [0x1234, 0x5678]
binary_string3 = arr3.pack("S>2") # Packs two 16-bit unsigned integers, big-endian
# binary_string3 will be "\x12\x34\x56\x78" (depending on native endianness if S is used without > or <)
# Using "n2" would be more explicit for network (big-endian) byte order:
# binary_string3 = arr3.pack("n2") # Packs two 16-bit unsigned, network (big-endian) byte order
# binary_string3 will be "\x12\x34\x56\x78"
```

## `String#unpack`

The `String#unpack` method performs the reverse operation of `Array#pack`. It takes a binary string and a template string as input. It extracts data from the binary string according to the directives in the template and converts it into an array of Ruby objects.

**Example:**

```ruby
binary_data = "test"
arr = binary_data.unpack("aaaa")
# arr will be ["t", "e", "s", "t"]

binary_data2 = "\x01\x02\x03"
arr2 = binary_data2.unpack("C3") # Unpacks three 8-bit unsigned integers
# arr2 will be [1, 2, 3]

binary_data3 = "Hello\x00World"
arr3 = binary_data3.unpack("Z*Z*") # Unpacks two null-terminated strings
# arr3 will be ["Hello", "World"]

# BER (Basic Encoding Rules) example
arr4 = [127, 128, 16383]
binary_data4 = arr4.pack("w*") # BER-encodes integers of varying sizes
# binary_data4 will contain BER-compressed data
arr4_unpacked = binary_data4.unpack("w*") # Decodes BER data back to integers
# arr4_unpacked will be [127, 128, 16383]
```

## Supported Template Directives

The template string is a sequence of characters that specify the type and format of the data to be packed or unpacked. Each directive can be followed by a count (e.g., "C4" for four 8-bit unsigned integers) or `*` to consume all remaining items/bytes.

Here is a list of supported template characters and their meanings:

| Directive | Description                                                                                             |
| --------- | ------------------------------------------------------------------------------------------------------- |
| `A`       | Arbitrary binary string (space padded, count is width)                                                  |
| `a`       | Arbitrary binary string (null padded, count is width)                                                   |
| `B`       | Bit string (descending bit order)                                                                       |
| `b`       | Bit string (ascending bit order)                                                                        |
| `C`       | 8-bit unsigned integer (unsigned char)                                                                  |
| `c`       | 8-bit signed integer (signed char)                                                                      |
| `D`, `d`  | Double-precision float, native format (64-bit)                                                          |
| `E`       | Double-precision float, little-endian (64-bit)                                                          |
| `e`       | Single-precision float, little-endian (32-bit)                                                          |
| `F`, `f`  | Single-precision float, native format (32-bit)                                                          |
| `G`       | Double-precision float, network (big-endian) byte order (64-bit)                                        |
| `g`       | Single-precision float, network (big-endian) byte order (32-bit)                                        |
| `H`       | Hex string (high nibble first)                                                                          |
| `h`       | Hex string (low nibble first)                                                                           |
| `I`       | Unsigned integer, native endian (`unsigned int` in C)                                                   |
| `i`       | Signed integer, native endian (`int` in C)                                                              |
| `J`       | Unsigned integer, native endian (`uintptr_t` in C)                                                      |
| `j`       | Signed integer, native endian (`intptr_t` in C)                                                         |
| `L`       | 32-bit unsigned integer, native endian (`uint32_t`)                                                     |
| `l`       | 32-bit signed integer, native endian (`int32_t`)                                                        |
| `m`       | Base64 encoded string (see RFC 2045, count is input bytes for pack, output chars for unpack)            |
| `N`       | 32-bit unsigned integer, network (big-endian) byte order                                                |
| `n`       | 16-bit unsigned integer, network (big-endian) byte order                                                |
| `Q`       | 64-bit unsigned integer, native endian (`uint64_t`)                                                     |
| `q`       | 64-bit signed integer, native endian (`int64_t`)                                                        |
| `S`       | 16-bit unsigned integer, native endian (`uint16_t`) (Use `S>` for big-endian, `S<` for little-endian)   |
| `s`       | 16-bit signed integer, native endian (`int16_t`) (Use `s>` for big-endian, `s<` for little-endian)      |
| `u`       | UU-encoded string (Unix-to-Unix encoding)                                                               |
| `U`       | UTF-8 character                                                                                         |
| `V`       | 32-bit unsigned integer, VAX (little-endian) byte order                                                 |
| `v`       | 16-bit unsigned integer, VAX (little-endian) byte order                                                 |
| `w`       | BER-compressed integer (variable length encoding)                                                       |
| `x`       | Null byte (skip forward one byte)                                                                       |
| `X`       | Back up one byte                                                                                        |
| `Z`       | Null-terminated string (when unpacking, reads until NULL; when packing, appends a NULL if count is `*`) |
| `@`       | Moves to absolute position (offset from the beginning of the string)                                    |

**Modifiers:**

Some directives can be followed by modifiers that affect their behavior:

- `>`: Big-endian (for `S`, `s`, `L`, `l`, `Q`, `q`, etc., when native order is not desired).
- `<`: Little-endian (for `S`, `s`, `L`, `l`, `Q`, `q`, etc., when native order is not desired).

These modifiers are typically used with integer and float types to specify byte order explicitly. For network byte order (big-endian), directives like `n` (16-bit) and `N` (32-bit) are commonly used.

## License

Copyright (c) 2012 Internet Initiative Japan Inc.
Copyright (c) 2017 mruby developers

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
