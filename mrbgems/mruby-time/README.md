# mruby-time

mruby-time is an mrbgem that provides a `Time` class for mruby, offering functionalities for time manipulation and representation. It is designed to be largely compatible with the Time class found in standard Ruby, following the ISO Ruby Time class specification.

## Purpose

The primary purpose of `mruby-time` is to enable mruby applications to:

- Work with specific points in time.
- Handle timezones, primarily UTC and the local system timezone.
- Perform time arithmetic.
- Format time information into human-readable strings.

## Functionality

### Creating Time Objects

You can create `Time` objects in several ways:

- **`Time.now`**: Returns a `Time` object representing the current time based on the system's clock.

  ```ruby
  t = Time.now
  ```

- **`Time.at(seconds_with_fraction)`**: Creates a `Time` object for the time `seconds_with_fraction` since the Epoch (January 1, 1970, 00:00:00 UTC). You can provide seconds as an integer or a float for sub-second precision.

  ```ruby
  t1 = Time.at(1678886400)       # Integer seconds
  t2 = Time.at(1678886400.5)   # Seconds with microseconds
  ```

- **`Time.local(year, month, day, hour, min, sec, usec)`** (or **`Time.mktime`**): Creates a `Time` object from the given components in the local timezone. Arguments beyond `year` are optional and default to minimum values (e.g., month 1, day 1, hour 0, etc.).

  ```ruby
  t = Time.local(2023, 3, 15, 12, 30, 0) # March 15, 2023, 12:30:00 local time
  ```

- **`Time.gm(year, month, day, hour, min, sec, usec)`** (or **`Time.utc`**): Creates a `Time` object from the given components in UTC. Arguments are optional similar to `Time.local`.

  ```ruby
  t = Time.gm(2023, 3, 15, 12, 30, 0)    # March 15, 2023, 12:30:00 UTC
  ```

### Getting Time Components

Once you have a `Time` object, you can extract its components:

- `t.year`: Returns the year.
- `t.month` (or `t.mon`): Returns the month of the year (1-12).
- `t.day` (or `t.mday`): Returns the day of the month (1-31).
- `t.hour`: Returns the hour of the day (0-23).
- `t.min`: Returns the minute of the hour (0-59).
- `t.sec`: Returns the second of the minute (0-59).
- `t.usec`: Returns the microsecond of the second (0-999999).
- `t.wday`: Returns the day of the week (0 for Sunday, 1 for Monday, ..., 6 for Saturday).
- `t.yday`: Returns the day of the year (1-366).
- `t.zone`: Returns the timezone name ("UTC" or local timezone offset like "+0900").

### Timezone Conversions

- `t.utc?` (or `t.gmt?`): Returns `true` if the `Time` object is in UTC.
- `t.utc` (or `t.gmtime`): Converts the `Time` object to UTC and returns `self`.
- `t.getutc` (or `t.getgm`): Returns a new `Time` object representing the same point in time as `t`, but in UTC.
- `t.localtime`: Converts the `Time` object to the local timezone and returns `self`.
- `t.getlocal`: Returns a new `Time` object representing the same point in time as `t`, but in the local timezone.

### Time Arithmetic

- **Addition (`+`)**: Adds a duration (in seconds) to a `Time` object, returning a new `Time` object.

  ```ruby
  t1 = Time.now
  t2 = t1 + 60 # 60 seconds later
  ```

- **Subtraction (`-`)**:
  - Subtracting a duration (in seconds) from a `Time` object returns a new `Time` object.
  - Subtracting another `Time` object returns the difference in seconds as a Float.

  ```ruby
  t1 = Time.now
  t_earlier = t1 - 3600 # One hour earlier

  t2 = Time.local(2023, 1, 1)
  t3 = Time.local(2023, 1, 2)
  difference_seconds = t3 - t2 # Returns 86400.0
  ```

### Formatting Time

- **`t.to_s`**: Returns a string representation of the time (e.g., "2023-03-15 10:30:00 +0000"). The format may vary slightly.
- **`t.inspect`**: Similar to `to_s`, provides a string representation.
- **`t.asctime`** (or **`t.ctime`**): Returns a canonical string representation (e.g., "Wed Mar 15 10:30:00 2023").
- **`t.to_i`**: Returns the number of seconds since the Epoch as an integer.
- **`t.to_f`**: Returns the number of seconds since the Epoch as a float (including microseconds).

## Compatibility

`mruby-time` aims to be compatible with the standard Ruby `Time` class as defined by ISO/IEC 30170:2012 (Ruby Language Specification). However, due to the nature of embedded systems and the mruby environment, there might be minor differences or limitations, especially concerning timezone data complexity beyond UTC and local system time.

Refer to the source code and tests for detailed behavior.
