class PWM
  def initialize(pin, frequency: 0, duty: 50)
    @pin = pin
    __init(@pin)
    @frequency = frequency.to_f
    @duty = duty.to_f
    frequency(@frequency)
  end
end
