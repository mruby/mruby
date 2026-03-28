# hw-spi - SPI peripheral interface for mruby

This gem provides the `SPI` class for Serial Peripheral Interface
communication from mruby. It is designed for embedded platforms
such as ESP32 and RP2040.

## Architecture

Platform-specific HAL implementations are in `ports/` directories:

- `ports/esp32/` - ESP32 using ESP-IDF SPI master driver
- `ports/rp2040/` - RP2040 using Pico SDK

The build system automatically compiles matching port sources based
on `conf.ports` setting.

## Build Configuration

```ruby
MRuby::CrossBuild.new('esp32') do |conf|
  conf.ports :esp32
  conf.gem core: 'hw-spi'
end
```

## Ruby API

### SPI.new

```ruby
spi = SPI.new(
  unit:      :RP2040_SPI0,     # SPI unit name (required)
  frequency: 100_000,          # clock frequency in Hz (default: 100kHz)
  sck_pin:   -1,               # SCK GPIO pin (default: platform default)
  copi_pin:  -1,               # COPI/MOSI GPIO pin
  cipo_pin:  -1,               # CIPO/MISO GPIO pin
  cs_pin:    -1,               # CS GPIO pin (-1 for manual control)
  mode:      0,                # SPI mode 0-3 (default: 0)
  first_bit: SPI::MSB_FIRST    # bit order (default: MSB_FIRST)
)
```

#### Unit Names

| Platform | Available Units                                     |
|----------|-----------------------------------------------------|
| ESP32    | `:ESP32_SPI2_HOST`, `:ESP32_HSPI_HOST`,             |
|          | `:ESP32_SPI3_HOST`\*, `:ESP32_VSPI_HOST`\*          |
| RP2040   | `:RP2040_SPI0`, `:RP2040_SPI1`                      |

\*SPI3 availability depends on ESP32 variant.

### SPI#write(*data)

Write data to the SPI bus. Data can be Integer, Array, or String.

```ruby
spi.write(0x01, [0x02, 0x03])
```

### SPI#read(len, tx_value = 0)

Read `len` bytes. Optionally specify the value to transmit during
read.

```ruby
data = spi.read(4)         # transmits 0x00 while reading
data = spi.read(4, 0xFF)   # transmits 0xFF while reading
```

### SPI#transfer(*data, additional_read_bytes: 0)

Full-duplex transfer. Sends data and returns received bytes.
Use `additional_read_bytes:` to append zero-filled read bytes.

```ruby
# Send 1 byte command, read 4 bytes response
rx = spi.transfer(0x9F, additional_read_bytes: 4)
```

### SPI#select / SPI#deselect

Manually control the CS pin (when using GPIO-based chip select).

```ruby
spi.select do |s|
  s.write(0x01)
  data = s.read(4)
end  # CS automatically deasserted
```

## HAL Interface

To add support for a new platform, create a `ports/<name>/`
directory and implement the following C functions declared in
`<mruby/spi.h>`:

```c
int mrb_spi_unit_name_to_num(const char *name);
mrb_spi_status mrb_spi_init(mrb_spi_info *info);
int mrb_spi_read(mrb_spi_info *info, uint8_t *dst, size_t len,
                 uint8_t tx_val);
int mrb_spi_write(mrb_spi_info *info, const uint8_t *src, size_t len);
int mrb_spi_transfer(mrb_spi_info *info, const uint8_t *tx,
                     uint8_t *rx, size_t len);
```

The `mrb_spi_info` struct contains all configuration (unit, pins,
frequency, mode, bit order) and is passed to every HAL call.

## License

MIT
