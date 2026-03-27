class UART
  PARITY_NONE = 0
  PARITY_EVEN = 1
  PARITY_ODD  = 2
  FLOW_CONTROL_NONE    = 0
  FLOW_CONTROL_RTS_CTS = 1

  attr_reader :baudrate

  def initialize(unit:, tx_pin: -1, rx_pin: -1, baudrate: 9600,
                 data_bits: 8, stop_bits: 1, parity: PARITY_NONE,
                 flow_control: FLOW_CONTROL_NONE, rx_buffer_size: 256)
    __open_rx_buffer(rx_buffer_size)
    @unit_num = __open_connection(unit.to_s, tx_pin, rx_pin)
    @baudrate = __set_baudrate(baudrate)
    __set_format(data_bits, stop_bits, parity)
    set_flow_control(flow_control)
    @line_ending = "\n"
  end

  def setmode(baudrate: nil, data_bits: nil, stop_bits: nil,
              parity: nil, flow_control: nil)
    @baudrate = __set_baudrate(baudrate) if baudrate
    __set_format(data_bits || 8, stop_bits || 1, parity || PARITY_NONE)
    set_flow_control(flow_control || FLOW_CONTROL_NONE)
    self
  end

  def line_ending=(ending)
    unless ["\n", "\r", "\r\n"].include?(ending)
      raise ArgumentError, "invalid line ending"
    end
    @line_ending = ending
  end

  def puts(str)
    write str
    write @line_ending unless str.end_with?(@line_ending)
    nil
  end

  private

  def set_flow_control(mode)
    case mode
    when FLOW_CONTROL_NONE
      __set_flow_control(false, false)
    when FLOW_CONTROL_RTS_CTS
      __set_flow_control(true, true)
    else
      raise ArgumentError, "invalid flow control mode"
    end
  end
end
