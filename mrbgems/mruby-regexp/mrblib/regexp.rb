class Regexp
  def self.compile(pattern, *args)
    new(pattern, *args)
  end

  def options
    @flags.to_i
  end

  # $1-$9 convenience methods via $~
  def self.last_match(n = nil)
    md = $~
    return md if n.nil?
    md ? md[n] : nil
  end
end
