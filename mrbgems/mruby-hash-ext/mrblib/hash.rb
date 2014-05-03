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

  ##
  #  call-seq:
  #     hsh.delete_if {| key, value | block }  -> hsh
  #     hsh.delete_if                          -> an_enumerator
  #
  #  Deletes every key-value pair from <i>hsh</i> for which <i>block</i>
  #  evaluates to <code>true</code>.
  #
  #  If no block is given, an enumerator is returned instead.
  #
  #     h = { "a" => 100, "b" => 200, "c" => 300 }
  #     h.delete_if {|key, value| key >= "b" }   #=> {"a"=>100}
  #

  def delete_if(&block)
    return to_enum :delete_if unless block_given?

    self.each do |k, v|
      self.delete(k) if block.call(k, v)
    end 
    self
  end
end
