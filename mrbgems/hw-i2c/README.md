# hw-i2c - I2C peripheral interface for mruby

This gem provides the `I2C` class for communicating with I2C devices from mruby. It is designed for embedded platforms such as ESP32 and RP2040.

## Architecture

The I2C support is split into a common gem and platform-specific HAL gems:

- **hw-i2c** (this gem) - Ruby API, C bindings, and HAL function declarations
- **hw-esp32-i2c** - HAL implementation for ESP32 (using ESP-IDF I2C master driver)
- **hw-rp2040-i2c** - HAL implementation for RP2040 (using Pico SDK)

The platform gems depend on hw-i2c, so you only need to specify the platform gem in your build configuration.

## Build Configuration

```ruby
# For ESP32
MRuby::CrossBuild.new('esp32') do |conf|
  # ...
  conf.gem "#{root}/mrbgems/hw-esp32-i2c"
end

# For RP2040
MRuby::CrossBuild.new('rp2040') do |conf|
  # ...
  conf.gem "#{root}/mrbgems/hw-rp2040-i2c"
end
```

## Ruby API

### I2C.new

```ruby
i2c = I2C.new(
  unit:      :ESP32_I2C0,   # I2C unit name (platform-specific, required)
  frequency: 100_000,       # bus frequency in Hz (default: 100kHz)
  sda_pin:   21,            # SDA GPIO pin number (default: -1 for platform default)
  scl_pin:   22,            # SCL GPIO pin number (default: -1 for platform default)
  timeout:   500            # default timeout in ms (default: 500)
)
```

#### Unit Names

| Platform | Available Units                 |
|----------|---------------------------------|
| ESP32    | `:ESP32_I2C0`, `:ESP32_I2C1`    |
| RP2040   | `:RP2040_I2C0`, `:RP2040_I2C1`  |

On RP2040, if `sda_pin` or `scl_pin` is -1, the Pico SDK default pins are used.

### I2C#write

Write data to an I2C device.

```ruby
i2c.write(addr, *data, timeout: 500)
```

- `addr` - 7-bit I2C device address (Integer)
- `data` - one or more data arguments, each can be:
  - **Integer** - a single byte (0-255)
  - **Array of Integer** - multiple bytes
  - **String** - raw bytes
- `timeout:` - optional timeout in ms (overrides instance default)
- Returns the number of bytes written (Integer)
- Raises `IOError` on failure

```ruby
# Write a single byte
i2c.write(0x3C, 0x00)

# Write multiple bytes
i2c.write(0x3C, 0x00, [0xAE, 0xD5, 0x80])

# Write a string
i2c.write(0x3C, "hello")

# Mix data types
i2c.write(0x3C, 0x40, [0x01, 0x02], "data")
```

### I2C#read

Read data from an I2C device. Optionally write data before reading (repeated START).

```ruby
i2c.read(addr, length, *write_data, timeout: 500)
```

- `addr` - 7-bit I2C device address (Integer)
- `length` - number of bytes to read (Integer, must be positive)
- `write_data` - optional data to write before reading (same format as `write`). When provided, the gem performs a write-then-read transaction using I2C repeated START condition. This is the standard way to read from a specific register.
- `timeout:` - optional timeout in ms (overrides instance default)
- Returns the data read (String)
- Raises `IOError` on failure, `ArgumentError` if length <= 0

```ruby
# Simple read (2 bytes from device)
data = i2c.read(0x50, 2)

# Register read: write register address 0x00, then read 2 bytes
data = i2c.read(0x50, 2, 0x00)

# Multi-byte register address
data = i2c.read(0x50, 4, [0x00, 0x10])
```

### I2C#scan

Scan the I2C bus for responsive devices.

```ruby
i2c.scan(timeout: 500)
```

- `timeout:` - optional timeout per probe in ms
- Returns an Array of 7-bit addresses (Integer) that responded

```ruby
found = i2c.scan
# => [0x3C, 0x50, 0x68]
```

## HAL Interface

To add support for a new platform, create a gem (e.g., `hw-myboard-i2c`) that depends on `hw-i2c` and implements the following C functions declared in `<mruby/i2c.h>`:

```c
/* Convert platform-specific unit name string to unit number.
   Return MRB_I2C_ERROR_UNIT for unknown names. */
int mrb_i2c_unit_name_to_num(const char *name);

/* Initialize an I2C bus unit.
   sda/scl: GPIO pin numbers (-1 for platform default if available).
   Return MRB_I2C_OK on success. */
mrb_i2c_status mrb_i2c_init(int unit, uint32_t freq, int8_t sda, int8_t scl);

/* Read len bytes from device at addr.
   Return number of bytes read on success, negative on error. */
int mrb_i2c_read(int unit, uint8_t addr, uint8_t *dst, size_t len,
                 uint32_t timeout_us);

/* Write len bytes to device at addr.
   Return number of bytes written on success, negative on error. */
int mrb_i2c_write(int unit, uint8_t addr, const uint8_t *src, size_t len,
                  uint32_t timeout_us);

/* Atomic write-then-read using repeated START condition.
   Write wlen bytes from src, then read rlen bytes into dst.
   Return number of bytes read on success, negative on error. */
int mrb_i2c_write_read(int unit, uint8_t addr,
                       const uint8_t *src, size_t wlen,
                       uint8_t *dst, size_t rlen,
                       uint32_t timeout_us);
```

The gem must also provide empty `mrb_<gemname>_gem_init()` and `mrb_<gemname>_gem_final()` functions (with hyphens replaced by underscores).

### Error Codes

```c
typedef enum {
  MRB_I2C_OK             =  0,
  MRB_I2C_ERROR_UNIT     = -1,  /* invalid or uninitialized unit */
  MRB_I2C_ERROR_TIMEOUT  = -2,  /* communication timeout */
  MRB_I2C_ERROR_NACK     = -3,  /* device did not acknowledge */
} mrb_i2c_status;
```

## License

MIT
