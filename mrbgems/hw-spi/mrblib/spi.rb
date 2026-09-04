class SPI
  MSB_FIRST = 1
  LSB_FIRST = 0
  DEFAULT_FREQUENCY = 100_000

  def select
    @cs&.write 0
    if block_given?
      begin
        yield self
      ensure
        deselect
      end
    end
  end

  def deselect
    @cs&.write 1
  end
end
