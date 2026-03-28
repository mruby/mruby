# hw-adc - ADC peripheral interface for mruby

This gem provides the `ADC` class for reading analog input from
mruby. It is designed for embedded platforms such as ESP32 and
RP2040.

## Architecture

Platform-specific HAL implementations are in `ports/` directories:

- `ports/esp32/` - ESP32 using ESP-IDF ADC oneshot driver
- `ports/rp2040/` - RP2040 using Pico SDK ADC hardware

## Build Configuration

```ruby
MRuby::CrossBuild.new('esp32') do |conf|
  conf.ports :esp32
  conf.gem core: 'hw-adc'
end
```

## Ruby API

### ADC.new

```ruby
adc = ADC.new(pin)
```

- `pin` - ADC-capable GPIO pin number (Integer)
- Raises `ArgumentError` if the pin is not valid for ADC

### ADC#read / ADC#read_voltage

Read the analog value as voltage (Float).

```ruby
voltage = adc.read           # => 1.65
voltage = adc.read_voltage   # same
```

### ADC#read_raw

Read the raw ADC value (Integer, typically 0-4095 for 12-bit).

```ruby
raw = adc.read_raw   # => 2048
```

### ADC#input

Returns the ADC input channel number assigned during initialization.

## HAL Interface

To add support for a new platform, create a `ports/<name>/`
directory and implement the following C functions declared in
`<mruby/adc.h>`:

```c
int      mrb_adc_init(uint8_t pin);
uint32_t mrb_adc_read_raw(uint8_t input);
float    mrb_adc_read_voltage(uint8_t input);
```

- `mrb_adc_init` returns the input channel number (>= 0) on
  success, negative on error
- `input` parameter for read functions is the value returned by
  `mrb_adc_init`

## License

MIT
