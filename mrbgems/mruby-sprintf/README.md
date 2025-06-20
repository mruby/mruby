# mruby-sprintf

This mrbgem provides `sprintf` functionality for mruby. It allows you to format strings according to a specified format string, similar to the `sprintf` function in C or other languages.

## Functionality

- Provides the `sprintf` kernel function.
- Adds the `%` operator to the `String` class as an alias for `sprintf`.

## Usage

You can use `sprintf` or the `String#%` operator for string formatting:

```ruby
# Using sprintf
str = sprintf("Hello, %s! You are %d years old.", "World", 30)
puts str # Output: Hello, World! You are 30 years old.

# Using String#%
str2 = "The value is %.2f" % 12.3456
puts str2 # Output: The value is 12.35

# Using String#% with an array of arguments
str3 = "%d + %d = %d" % [1, 2, 1+2]
puts str3 # Output: 1 + 2 = 3
```

## Format Specifiers

This mrbgem supports a wide range of format specifiers, flags, width, and precision options. For a comprehensive list and detailed explanation of all supported format specifiers and their behavior, please refer to the extensive comments within the C source file: [`src/sprintf.c`](src/sprintf.c).

## Format Specifiers Details

The `sprintf` function in this mrbgem supports a variety of field type characters that control how arguments are interpreted and formatted.

### Integer Formats

| Field | Description                                                                                                             |
| :---- | :---------------------------------------------------------------------------------------------------------------------- |
| `b`   | Convert argument as a binary number. Negative numbers will be displayed as a two's complement prefixed with `..1`.      |
| `B`   | Equivalent to `b`, but uses an uppercase `0B` for prefix in the alternative format (when `#` flag is used).             |
| `d`   | Convert argument as a decimal number.                                                                                   |
| `i`   | Identical to `d`.                                                                                                       |
| `o`   | Convert argument as an octal number. Negative numbers will be displayed as a two's complement prefixed with `..7`.      |
| `u`   | Identical to `d`. (In many `sprintf` implementations, `u` is for unsigned decimal, but here it behaves like `d`).       |
| `x`   | Convert argument as a hexadecimal number. Negative numbers will be displayed as a two's complement prefixed with `..f`. |
| `X`   | Equivalent to `x`, but uses uppercase letters for hexadecimal digits (e.g., `0XFF` vs `0xff`).                          |

### Float Formats

| Field | Description                                                                                                                                             |
| :---- | :------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `e`   | Convert floating-point argument into exponential notation (e.g., `[-]d.dddddde[+-]dd`). Precision specifies digits after the decimal point (default 6). |
| `E`   | Equivalent to `e`, but uses an uppercase `E` for the exponent.                                                                                          |
| `f`   | Convert floating-point argument as `[-]ddd.dddddd`. Precision specifies digits after the decimal point.                                                 |
| `g`   | Converts a floating-point number using `e` or `f` form based on exponent and precision. Precision specifies significant digits.                         |
| `G`   | Equivalent to `g`, but uses an uppercase `E` in exponent form if applicable.                                                                            |

### Other Formats

| Field | Description                                                                                                     |
| :---- | :-------------------------------------------------------------------------------------------------------------- |
| `c`   | Argument is treated as the numeric code for a single character or a single character string itself.             |
| `p`   | Argument's `inspect` method is called, and the result is substituted.                                           |
| `s`   | Argument is a string to be substituted. If precision is specified, at most that many characters will be copied. |
| `%`   | A literal percent sign (`%`) itself will be displayed. No argument is taken.                                    |

### Flags

Flags modify the behavior of the format specifiers:

| Flag       | Applies to                          | Meaning                                                                                                                                                                                                                                                                                       |
| :--------- | :---------------------------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `space`    | Numeric Formats (bBdiouxX, aAeEfgG) | Leave a space at the start of non-negative numbers. For `o`, `x`, `X`, `b`, `B`, uses a minus sign with absolute value for negative values.                                                                                                                                                   |
| `(digit)$` | All                                 | Specifies the absolute argument number for this field (e.g., `%2$d` uses the second argument). Cannot be mixed with relative argument numbers.                                                                                                                                                |
| `#`        | bBoxX, aAeEfgG                      | Use an alternative format. For `o`, increases precision to make the first digit `0` if not formatted as complement. For `x`, `X`, `b`, `B` (non-zero), prefixes with `0x`, `0X`, `0b`, `0B`. For `e`, `E`, `f`, `g`, `G`, forces a decimal point. For `g`, `G`, do not remove trailing zeros. |
| `+`        | Numeric Formats                     | Add a leading plus sign (`+`) to non-negative numbers. For `o`, `x`, `X`, `b`, `B`, uses a minus sign with absolute value for negative values.                                                                                                                                                |
| `-`        | All                                 | Left-justify the result of this conversion.                                                                                                                                                                                                                                                   |
| `0` (zero) | Numeric Formats                     | Pad with zeros, not spaces. For `o`, `x`, `X`, `b`, `B`, radix-1 is used for negative numbers formatted as complements.                                                                                                                                                                       |
| `*`        | All                                 | Use the next argument as the field width. If negative, left-justifies. If `*` is followed by a number and `$`, use that argument as width (e.g., `%*1$d`).                                                                                                                                    |

## Further Examples (from C source comments)

Here are more examples illustrating the use of flags, width, and precision, adapted from the comments in `src/sprintf.c`.

### Examples of Flags

```ruby
# '+' and space flag specifies the sign of non-negative numbers.
puts sprintf("%d", 123)  #=> "123"
puts sprintf("%+d", 123) #=> "+123"
puts sprintf("% d", 123) #=> " 123"

# '#' flag for 'o' increases number of digits to show '0'.
# '+' and space flag changes format of negative numbers.
puts sprintf("%o", 123)   #=> "173"
puts sprintf("%#o", 123)  #=> "0173"
puts sprintf("%+o", -123) #=> "-173"
puts sprintf("%o", -123)  #=> "..7605"
puts sprintf("%#o", -123) #=> "..7605"

# '#' flag for 'x' add a prefix '0x' for non-zero numbers.
# '+' and space flag disables complements for negative numbers.
puts sprintf("%x", 123)   #=> "7b"
puts sprintf("%#x", 123)  #=> "0x7b"
puts sprintf("%+x", -123) #=> "-7b"
puts sprintf("%x", -123)  #=> "..f85"
puts sprintf("%#x", -123) #=> "0x..f85" # Note: mruby's output might differ slightly for complements with #
puts sprintf("%#x", 0)    #=> "0"

# '#' for 'X' uses the prefix '0X'.
puts sprintf("%X", 123)  #=> "7B"
puts sprintf("%#X", 123) #=> "0X7B"

# '#' flag for 'b' add a prefix '0b' for non-zero numbers.
# '+' and space flag disables complements for negative numbers.
puts sprintf("%b", 123)   #=> "1111011"
puts sprintf("%#b", 123)  #=> "0b1111011"
puts sprintf("%+b", -123) #=> "-1111011"
puts sprintf("%b", -123)  #=> "..10000101"
puts sprintf("%#b", -123) #=> "0b..10000101" # Note: mruby's output might differ slightly for complements with #
puts sprintf("%#b", 0)    #=> "0"

# '#' for 'B' uses the prefix '0B'.
puts sprintf("%B", 123)  #=> "1111011"
puts sprintf("%#B", 123) #=> "0B1111011"

# '#' for 'e' forces to show the decimal point.
puts sprintf("%.0e", 1)  #=> "1e+00"
puts sprintf("%#.0e", 1) #=> "1.e+00"

# '#' for 'f' forces to show the decimal point.
puts sprintf("%.0f", 1234)  #=> "1234"
puts sprintf("%#.0f", 1234) #=> "1234."

# '#' for 'g' forces to show the decimal point.
# It also disables stripping lowest zeros.
puts sprintf("%g", 123.4)   #=> "123.4"
puts sprintf("%#g", 123.4)  #=> "123.400"
puts sprintf("%g", 123456)  #=> "123456"
puts sprintf("%#g", 123456) #=> "123456."
```

### Examples of Width

The width specifies the minimum number of characters that will be written.
Padding is typically done with spaces, or with zeros if the `0` flag is used.

```ruby
# padding is done by spaces, width=20
# 0 or radix-1.             <------------------>
puts sprintf("%20d", 123)   #=> "                 123"
puts sprintf("%+20d", 123)  #=> "                +123"
puts sprintf("%020d", 123)  #=> "00000000000000000123"
puts sprintf("%+020d", 123) #=> "+0000000000000000123"
puts sprintf("% 020d", 123) #=> " 0000000000000000123"
puts sprintf("%-20d", 123)  #=> "123                 "
puts sprintf("%-+20d", 123) #=> "+123                "
puts sprintf("%- 20d", 123) #=> " 123                "
puts sprintf("%020x", -123) #=> "..ffffffffffffffff85" # Output for negative hex with 0-padding can vary
```

### Examples of Precision

For numeric fields, precision controls the number of decimal places or minimum number of digits.
For strings, it determines the maximum number of characters.

```ruby
# precision for 'd', 'o', 'x' and 'b' is minimum number of digits
#                                     <------>
puts sprintf("%20.8d", 123)  #=> "            00000123"
puts sprintf("%20.8o", 123)  #=> "            00000173"
puts sprintf("%20.8x", 123)  #=> "            0000007b"
puts sprintf("%20.8b", 123)  #=> "            01111011"
puts sprintf("%20.8d", -123) #=> "           -00000123"
puts sprintf("%20.8o", -123) #=> "            ..777605" # Complemented output
puts sprintf("%20.8x", -123) #=> "            ..ffff85" # Complemented output
puts sprintf("%20.8b", -11)  #=> "            ..110101" # Complemented output

# "0x" and "0b" for '#x' and '#b' is not counted for precision,
# but "0" for '#o' is counted.        <------>
puts sprintf("%#20.8d", 123)  #=> "            00000123"
puts sprintf("%#20.8o", 123)  #=> "            00000173" # '#' with 'o' and precision
puts sprintf("%#20.8x", 123)  #=> "          0x0000007b"
puts sprintf("%#20.8b", 123)  #=> "          0b01111011"
puts sprintf("%#20.8d", -123) #=> "           -00000123"
puts sprintf("%#20.8o", -123) #=> "            ..777605" # Complemented output
puts sprintf("%#20.8x", -123) #=> "          0x..ffff85" # Complemented output with #
puts sprintf("%#20.8b", -11)  #=> "          0b..110101" # Complemented output with #

# precision for 'e' is number of digits after the decimal point
#                                     <------>
puts sprintf("%20.8e", 1234.56789) #=> "      1.23456789e+03"

# precision for 'f' is number of digits after the decimal point
#                                     <------>
puts sprintf("%20.8f", 1234.56789) #=> "       1234.56789000"

# precision for 'g' is number of significant digits
#                                     <------->
puts sprintf("%20.8g", 1234.56789) #=> "           1234.5679"
#                                     <------->
puts sprintf("%20.8g", 123456789)  #=> "       1.2345679e+08"

# precision for 's' is maximum number of characters
#                                     <------>
puts sprintf("%20.8s", "string test") #=> "            string t"
```

### General Examples

```ruby
puts sprintf("%d %04x", 123, 123)               #=> "123 007b"
puts sprintf("%08b '%4s'", 123, 123)            #=> "01111011 ' 123'" # Note: mruby output for %s with number might differ
puts sprintf("%1$*2$s %2$d %1$s", "hello", 8)   #=> "   hello 8 hello"
puts sprintf("%1$*2$s %2$d", "hello", -8)       #=> "hello    -8"
puts sprintf("%+g:% g:%-g", 1.23, 1.23, 1.23)   #=> "+1.23: 1.23:1.23"
puts sprintf("%u", -123)                        #=> "-123"
```

### Named References

For more complex formatting, Ruby supports a reference by name.
`%<name>s` style uses format style, but `%{name}` style doesn't (in standard Ruby, mruby might vary).

```ruby
puts sprintf("%<foo>d : %<bar>f", { :foo => 1, :bar => 2.0 }) # Using a float for %f
#=> "1 : 2.000000"

# The %{name} style might behave differently or not be supported in the same way as CRuby.
# Standard CRuby example:
# puts sprintf("%{foo}f", { :foo => 1 }) # => "1f" (This specific syntax might not apply or work in mruby)
# For mruby, stick to %<name>type for named arguments:
puts sprintf("%<foo>s", { :foo => "test" }) #=> "test"
```
