class Regexp
  def self.compile(pattern, *args)
    new(pattern, *args)
  end

  # Return named captures hash: {"name" => group_number, ...}
  def named_captures
    @named_captures || {}
  end

  def options
    @flags.to_i
  end

  def self.last_match(n = nil)
    md = $~
    return md if n.nil?
    md ? md[n] : nil
  end

  # named capture info is set via C create_matchdata
end

class MatchData
  # named_captures is implemented in C via md->regexp
end
