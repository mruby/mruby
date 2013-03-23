class Hash
  def merge!(other, &block)
    if block
      other.each_key{|k|
        self[k] = (self.has_key?(k))? block.call(k, self[k], other[k]): other[k]
      }
    else
      other.each_key{|k| self[k] = other[k]}
    end
    self
  end
end
