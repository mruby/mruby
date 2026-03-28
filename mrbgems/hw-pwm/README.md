# hw-pwm - PWM peripheral interface for mruby

This gem provides the `PWM` class for Pulse Width Modulation
output from mruby. It is designed for embedded platforms such as
ESP32 and RP2040.

## Architecture

Platform-specific HAL implementations are in `ports/` directories:

- `ports/esp32/` - ESP32 using LEDC peripheral
- `ports/rp2040/` - RP2040 using Pico SDK PWM hardware

## Build Configuration

```ruby
MRuby::CrossBuild.new('esp32') do |conf|
  conf.ports :esp32
  conf.gem core: 'hw-pwm'
end
```

## Ruby API

### PWM.new

```ruby
pwm = PWM.new(pin, frequency: 1000, duty: 50)
```

- `pin` - GPIO pin number (Integer)
- `frequency:` - frequency in Hz (default: 0, disabled)
- `duty:` - duty cycle in percent 0-100 (default: 50)

### PWM#frequency(freq)

Set frequency in Hz. Returns the frequency. Setting 0 disables
output.

```ruby
pwm.frequency(1000)   # 1 kHz
pwm.frequency(0)      # disable
```

### PWM#period_us(us)

Set period in microseconds. Returns the corresponding frequency.

```ruby
pwm.period_us(1000)   # 1ms period = 1 kHz
```

### PWM#duty(pct)

Set duty cycle in percent (0.0-100.0). Clamped to range.

```ruby
pwm.duty(75.0)
```

### PWM#pulse_width_us(us)

Set pulse width in microseconds. Duty cycle is calculated from
current frequency.

```ruby
pwm.pulse_width_us(500)   # 500us pulse width
```

## HAL Interface

To add support for a new platform, create a `ports/<name>/`
directory and implement the following C functions declared in
`<mruby/pwm.h>`:

```c
void mrb_pwm_init(uint32_t pin);
void mrb_pwm_set_freq_duty(uint32_t pin, float frequency, float duty);
void mrb_pwm_set_enabled(uint32_t pin, bool enabled);
```

- `frequency` is in Hz, `duty` is in percent (0-100)
- `mrb_pwm_set_enabled` is called with `false` when frequency is 0

## License

MIT
