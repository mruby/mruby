class I2C
  DEFAULT_FREQUENCY = 100_000 # Hz
  DEFAULT_TIMEOUT = 500 # ms

  def initialize(unit:, frequency: DEFAULT_FREQUENCY, sda_pin: -1, scl_pin: -1, timeout: DEFAULT_TIMEOUT)
    @timeout = timeout
    @unit_num = __init(unit.to_s, frequency, sda_pin, scl_pin)
  end

  def scan(timeout: @timeout)
    found = []
    (0x08..0x77).each do |addr|
      begin
        read(addr, 1, timeout: timeout)
        found << addr
      rescue IOError
      end
    end
    found
  end
end
