class GPIO
  IN         = 0x01
  OUT        = 0x02
  HIGH_Z     = 0x04
  PULL_UP    = 0x08
  PULL_DOWN  = 0x10
  OPEN_DRAIN = 0x20

  attr_reader :pin

  def initialize(pin, flags)
    @pin = pin
    __init(pin)
    setmode(flags)
  end

  def setmode(flags)
    dir = flags & (IN | OUT | HIGH_Z)
    n = (flags & IN != 0 ? 1 : 0) + (flags & OUT != 0 ? 1 : 0) + (flags & HIGH_Z != 0 ? 1 : 0)
    if n == 0
      raise ArgumentError, "specify one of IN, OUT, or HIGH_Z"
    elsif n > 1
      raise ArgumentError, "IN, OUT, and HIGH_Z are exclusive"
    end
    GPIO.set_dir_at(@pin, dir)

    pull = flags & (PULL_UP | PULL_DOWN)
    if pull == (PULL_UP | PULL_DOWN)
      raise ArgumentError, "PULL_UP and PULL_DOWN are exclusive"
    end
    GPIO.pull_up_at(@pin) if pull == PULL_UP
    GPIO.pull_down_at(@pin) if pull == PULL_DOWN
    GPIO.open_drain_at(@pin) if flags & OPEN_DRAIN != 0
    nil
  end

  def high?
    read != 0
  end

  def low?
    read == 0
  end
end
