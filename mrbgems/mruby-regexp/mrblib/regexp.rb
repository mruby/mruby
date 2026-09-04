class Regexp
  def self.compile(pattern, *args)
    new(pattern, *args)
  end

  # Return named captures hash: {"name" => [group_number, ...], ...}
  # @named_captures holds the internal name -> group list table, so a fresh
  # Hash is derived on every call and the caller cannot corrupt the table.
  def named_captures
    __check_initialized
    table = @named_captures
    return {} unless table
    result = {}
    table.each { |name, groups| result[name] = groups.dup }
    result
  end

  # Return the capture names in group order
  def names
    __check_initialized
    table = @named_captures
    table ? table.keys : []
  end

  # The two readers above are readings of a compiled pattern, and an object
  # from Regexp.allocate has none: no @source, no @flags, no capture table.
  # An empty table there is indistinguishable from a pattern that named
  # nothing, so they refuse it through the same guard the C readers use
  # (re_check_initialized() in src/regexp.c, reached by __check_initialized).

  # options is implemented in C (internal flags -> Ruby constants conversion)

  # named capture info is set via C create_matchdata
end

class MatchData
  # named_captures is implemented in C via md->regexp

  def names
    regexp.names
  end
end
