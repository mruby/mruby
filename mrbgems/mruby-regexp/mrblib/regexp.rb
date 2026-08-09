class Regexp
  def self.compile(pattern, *args)
    new(pattern, *args)
  end

  # Return named captures hash: {"name" => [group_number, ...], ...}
  # @named_captures holds the internal name -> group_number table, so a fresh
  # Hash is derived on every call and the caller cannot corrupt the table.
  def named_captures
    table = @named_captures
    return {} unless table
    result = {}
    table.each { |name, group| result[name] = [group] }
    result
  end

  # Return the capture names in group order
  def names
    table = @named_captures
    table ? table.keys : []
  end

  # options is implemented in C (internal flags -> Ruby constants conversion)

  def self.last_match(n = nil)
    md = $~
    return md if n.nil?
    md ? md[n] : nil
  end

  # named capture info is set via C create_matchdata
end

class MatchData
  # named_captures is implemented in C via md->regexp

  def names
    regexp.names
  end
end
