# mruby-strftime

`Time#strftime` implementation for mruby.

## Overview

This gem provides the `strftime` method for `Time` objects in mruby, enabling
formatted time string output using standard format directives.

## Usage

```ruby
require 'mruby-strftime'

t = Time.new(2023, 12, 25, 10, 30, 45)

t.strftime("%Y-%m-%d")           #=> "2023-12-25"
t.strftime("%H:%M:%S")           #=> "10:30:45"
t.strftime("%Y-%m-%d %H:%M:%S")  #=> "2023-12-25 10:30:45"
t.strftime("%A, %B %d, %Y")      #=> "Monday, December 25, 2023"
```

## Format Specifiers

The `strftime` method supports standard format directives. The exact set of
available directives depends on your system's `strftime(3)` implementation.
Common directives include:

### Date Components

- `%Y` - Year with century (e.g., 2023)
- `%y` - Year without century (00-99)
- `%m` - Month of the year (01-12)
- `%B` - Full month name (e.g., "December")
- `%b` - Abbreviated month name (e.g., "Dec")
- `%d` - Day of the month (01-31)
- `%j` - Day of the year (001-366)

### Time Components

- `%H` - Hour of the day, 24-hour clock (00-23)
- `%I` - Hour of the day, 12-hour clock (01-12)
- `%M` - Minute of the hour (00-59)
- `%S` - Second of the minute (00-60)
- `%p` - AM/PM indicator
- `%Z` - Timezone name or abbreviation

### Weekday

- `%A` - Full weekday name (e.g., "Monday")
- `%a` - Abbreviated weekday name (e.g., "Mon")
- `%w` - Day of the week (0-6, Sunday is 0)
- `%u` - Day of the week (1-7, Monday is 1)

### Combined Formats

- `%c` - Preferred date and time representation
- `%x` - Preferred date representation
- `%X` - Preferred time representation
- `%F` - ISO 8601 date format (equivalent to `%Y-%m-%d`)
- `%T` - ISO 8601 time format (equivalent to `%H:%M:%S`)

### Special Characters

- `%%` - Literal `%` character
- `%n` - Newline character
- `%t` - Tab character

## Features

### Timezone Support

The method respects the timezone of the `Time` object:

```ruby
t_utc = Time.utc(2023, 12, 25, 12, 0, 0)
t_local = Time.local(2023, 12, 25, 12, 0, 0)

t_utc.strftime("%Y-%m-%d %H:%M:%S %Z")
#=> "2023-12-25 12:00:00 UTC"

t_local.strftime("%Y-%m-%d %H:%M:%S %z")
#=> "2023-12-25 12:00:00 +0900" (example for JST)
```

### NUL Byte Handling

Unlike some implementations, mruby-strftime correctly handles NUL bytes (`\0`)
embedded in format strings, preserving them in the output:

```ruby
t = Time.gm(2023, 12, 25)
result = t.strftime("year\0%Y")
result.length  #=> 9 (includes the NUL byte)
```

This behavior maintains Ruby's string semantics where strings are length-based
rather than null-terminated.

## Implementation Details

### Buffer Management

The implementation uses dynamic buffer allocation:

- Initial buffer size: 64 bytes
- Maximum buffer size: 4096 bytes
- Automatically grows when needed for longer formatted strings

### Platform Compatibility

This gem uses the system's `strftime(3)` function for formatting. The exact
behavior of format directives may vary slightly between platforms, particularly
for:

- Locale-dependent formats (day/month names, date/time preferences)
- Timezone representations
- Platform-specific extensions

## Dependencies

- `mruby-time` - Required for `Time` class support

## Installation

Add to your `build_config.rb`:

```ruby
conf.gem :core => 'mruby-strftime'
```

Or include via gembox:

```ruby
conf.gembox 'stdlib-ext'  # Includes mruby-strftime
```

## License

MIT License - See mruby's main license file for details.
