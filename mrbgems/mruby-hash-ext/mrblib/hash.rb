class Hash
  def merge!(other, &block)
    raise "can't convert argument into Hash" unless other.respond_to?(:to_hash)
    if block
      other.each_key{|k|
        self[k] = (self.has_key?(k))? block.call(k, self[k], other[k]): other[k]
      }
    else
      other.each_key{|k| self[k] = other[k]}
    end
    self
  end

  alias each_pair each
  alias update merge!

  def fetch(key, none=NONE, &block)
    unless self.key?(key)
      if block
        block.call
      elsif none != NONE
        none
      else
        raise RuntimeError, "Key not found: #{key}"
      end
    else
      self[key]
    end
  end
end
