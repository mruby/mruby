# hw-uart - UART peripheral interface for mruby

This gem provides the `UART` class for serial communication from mruby.
It is designed for embedded platforms such as ESP32 and RP2040.

## Architecture

- **hw-uart** (this gem) - Ruby API, C bindings, ring buffer, and HAL
  function declarations
- **hw-esp32-uart** - HAL implementation for ESP32 (using ESP-IDF UART
  driver with FreeRTOS task for RX)
- **hw-rp2040-uart** - HAL implementation for RP2040 (using Pico SDK
  with IRQ-driven RX)

Received data is buffered in a ring buffer (allocated by the common
gem) that the platform HAL populates via interrupt or task. The ring
buffer size must be a power of two.

## Build Configuration

```ruby
# For ESP32
MRuby::CrossBuild.new('esp32') do |conf|
  # ...
  conf.gem "#{root}/mrbgems/hw-esp32-uart"
end

# For RP2040
MRuby::CrossBuild.new('rp2040') do |conf|
  # ...
  conf.gem "#{root}/mrbgems/hw-rp2040-uart"
end
```

## Ruby API

### Constants

| Constant                      | Value | Description         |
|-------------------------------|-------|---------------------|
| `UART::PARITY_NONE`           | `0`   | No parity           |
| `UART::PARITY_EVEN`           | `1`   | Even parity         |
| `UART::PARITY_ODD`            | `2`   | Odd parity          |
| `UART::FLOW_CONTROL_NONE`     | `0`   | No flow control     |
| `UART::FLOW_CONTROL_RTS_CTS`  | `1`   | Hardware flow ctrl  |

### UART.new

```ruby
uart = UART.new(
  unit:            :ESP32_UART1,    # UART unit name (required)
  tx_pin:          17,              # TX GPIO pin (default: -1)
  rx_pin:          16,              # RX GPIO pin (default: -1)
  baudrate:        9600,            # baud rate (default: 9600)
  data_bits:       8,               # 5-8 (default: 8)
  stop_bits:       1,               # 1-2 (default: 1)
  parity:          UART::PARITY_NONE,
  flow_control:    UART::FLOW_CONTROL_NONE,
  rx_buffer_size:  256              # must be power of two (default: 256)
)
```

#### Unit Names

| Platform | Available Units                                  |
|----------|--------------------------------------------------|
| ESP32    | `:ESP32_UART0`, `:ESP32_UART1`, `:ESP32_UART2`\* |
| RP2040   | `:RP2040_UART0`, `:RP2040_UART1`                 |

\*UART2 availability depends on ESP32 variant.

### Instance Methods

#### UART#write(str)

Write a string to the UART. Returns number of bytes written.

```ruby
uart.write("Hello\r\n")
```

#### UART#read(len = nil)

Read from the RX buffer. Returns `nil` if no data is available.

- Without argument: returns all available data
- With `len`: returns exactly `len` bytes, or `nil` if fewer are
  available

```ruby
data = uart.read       # all available
data = uart.read(10)   # exactly 10 bytes or nil
```

#### UART#readpartial(maxlen)

Read up to `maxlen` bytes from the RX buffer. Returns `nil` if empty.

```ruby
data = uart.readpartial(64)
```

#### UART#gets

Read a line (up to and including `"\n"`). Returns `nil` if no
complete line is available.

```ruby
line = uart.gets
```

#### UART#bytes_available

Returns the number of bytes in the RX buffer.

```ruby
n = uart.bytes_available
```

#### UART#puts(str)

Write string with line ending appended (if not already present).

```ruby
uart.puts("Hello")  # writes "Hello\n"
```

#### UART#flush

Wait for all TX data to be sent.

#### UART#clear_rx_buffer / UART#clear_tx_buffer

Discard buffered data.

#### UART#send_break(duration_ms = 100)

Send a UART break signal for the specified duration.

#### UART#setmode(baudrate:, data_bits:, stop_bits:, parity:, flow_control:)

Reconfigure UART parameters after initialization. All parameters are
optional.

#### UART#baudrate

Returns the current baud rate.

#### UART#line_ending=(ending)

Set the line ending used by `puts`. Must be `"\n"`, `"\r"`, or
`"\r\n"`.

## HAL Interface

To add support for a new platform, create a gem that depends on
`hw-uart` and implements the following C functions declared in
`<mruby/uart.h>`:

```c
int  mrb_uart_unit_name_to_num(const char *name);
mrb_uart_status mrb_uart_init(int unit, uint32_t tx_pin, uint32_t rx_pin,
                               mrb_uart_ringbuf *rxbuf);
uint32_t mrb_uart_set_baudrate(int unit, uint32_t baudrate);
void mrb_uart_set_format(int unit, uint32_t data_bits,
                          uint32_t stop_bits, uint8_t parity);
void mrb_uart_set_flow_control(int unit, bool cts, bool rts);
void mrb_uart_write(int unit, const uint8_t *src, size_t len);
void mrb_uart_flush(int unit);
void mrb_uart_send_break(int unit, uint32_t duration_ms);
void mrb_uart_clear_rx(int unit);
void mrb_uart_clear_tx(int unit);
```

The `rxbuf` parameter passed to `mrb_uart_init` is a ring buffer
allocated by the common gem. The platform must arrange for received
bytes to be pushed into it using `mrb_uart_ringbuf_push()` (e.g.,
from an interrupt handler or RTOS task).

## License

MIT
