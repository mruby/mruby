class ADC
  def initialize(pin)
    @input = __init(pin)
  end

  attr_reader :input
end
