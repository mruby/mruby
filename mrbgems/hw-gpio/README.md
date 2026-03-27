# hw-gpio - GPIO peripheral interface for mruby

This gem provides the `GPIO` class for controlling general-purpose
input/output pins from mruby.
It is designed for embedded platforms such as ESP32 and RP2040.

## Architecture

Platform-specific HAL implementations are in `ports/` directories:

- `ports/esp32/` - ESP32 using ESP-IDF GPIO driver
- `ports/rp2040/` - RP2040 using Pico SDK

The build system automatically compiles matching port sources based
on `conf.ports` setting.

## Build Configuration

```ruby
# For ESP32
MRuby::CrossBuild.new('esp32') do |conf|
  conf.ports :esp32
  conf.gem core: 'hw-gpio'
end

# For RP2040
MRuby::CrossBuild.new('rp2040') do |conf|
  conf.ports :rp2040
  conf.gem core: 'hw-gpio'
end
```

## Ruby API

### Constants (direction/mode flags)

These flags can be combined with bitwise OR.

| Flag                | Value  | Description                |
|---------------------|--------|----------------------------|
| `GPIO::IN`          | `0x01` | Input mode                 |
| `GPIO::OUT`         | `0x02` | Output mode                |
| `GPIO::HIGH_Z`      | `0x04` | High-impedance (tri-state) |
| `GPIO::PULL_UP`     | `0x08` | Enable pull-up resistor    |
| `GPIO::PULL_DOWN`   | `0x10` | Enable pull-down resistor  |
| `GPIO::OPEN_DRAIN`  | `0x20` | Enable open-drain output   |

### GPIO.new

```ruby
gpio = GPIO.new(pin, flags)
```

- `pin` - GPIO pin number (Integer)
- `flags` - direction and mode flags (combined with `|`)
- Exactly one of `IN`, `OUT`, or `HIGH_Z` must be specified
- `PULL_UP` and `PULL_DOWN` are mutually exclusive
- Raises `ArgumentError` on invalid flag combinations

```ruby
# Input with pull-up
button = GPIO.new(2, GPIO::IN | GPIO::PULL_UP)

# Output
led = GPIO.new(25, GPIO::OUT)
```

### Instance Methods

#### GPIO#read

Read the current pin value.

```ruby
val = gpio.read  # => 0 or 1
```

#### GPIO#write

Set the pin output value.

```ruby
gpio.write(1)  # set high
gpio.write(0)  # set low
```

- Raises `ArgumentError` if value is not 0 or 1

#### GPIO#high? / GPIO#low?

```ruby
gpio.high?  # => true if read != 0
gpio.low?   # => true if read == 0
```

#### GPIO#pin

```ruby
gpio.pin  # => pin number passed to new
```

#### GPIO#setmode

Reconfigure pin direction and mode flags after initialization.

```ruby
gpio.setmode(GPIO::OUT)
```

### Class Methods

These operate directly on pin numbers without creating an instance.

```ruby
GPIO.read_at(pin)           # => 0 or 1
GPIO.write_at(pin, val)     # set pin output (0 or 1)
GPIO.set_dir_at(pin, flags) # set direction (IN/OUT/HIGH_Z)
GPIO.pull_up_at(pin)        # enable pull-up
GPIO.pull_down_at(pin)      # enable pull-down
GPIO.open_drain_at(pin)     # enable open-drain
```

## HAL Interface

To add support for a new platform, create a `ports/<name>/`
directory and implement the following C functions declared in
`<mruby/gpio.h>`:

```c
void mrb_gpio_init(uint8_t pin);
void mrb_gpio_set_dir(uint8_t pin, uint8_t flags);
void mrb_gpio_pull_up(uint8_t pin);
void mrb_gpio_pull_down(uint8_t pin);
void mrb_gpio_open_drain(uint8_t pin);
int  mrb_gpio_read(uint8_t pin);
void mrb_gpio_write(uint8_t pin, uint8_t val);
```

The port sources are compiled automatically when the build
configuration includes a matching `conf.ports` tag.

## License

MIT
