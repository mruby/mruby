class << ENV
  def each(&block)
    return to_enum(:each) unless block
    keys.each do |k|
      v = self[k]
      block.call([k, v]) if v
    end
    self
  end
  alias each_pair each

  def each_key(&block)
    return to_enum(:each_key) unless block
    keys.each(&block)
    self
  end

  def each_value(&block)
    return to_enum(:each_value) unless block
    __values.each(&block)
    self
  end

  def values
    __values
  end

  def delete(key, &block)
    val = __delete(key)
    if val.nil? && block
      block.call(key)
    else
      val
    end
  end

  def clear
    __clear
    self
  end

  def fetch(key, *args, &block)
    val = self[key]
    return val unless val.nil?
    if block
      block.call(key)
    elsif args.length > 0
      args[0]
    else
      raise KeyError, "key not found: \"#{key}\""
    end
  end

  def store(key, val)
    self[key] = val
  end

  def to_h(&block)
    h = {}
    each do |k, v|
      if block
        k, v = block.call(k, v)
      end
      h[k] = v
    end
    h
  end

  def to_a
    each_pair.to_a
  end

  def inspect
    to_h.inspect
  end

  def empty?
    size == 0
  end

  def has_value?(val)
    __values.include?(val)
  end
  alias value? has_value?

  def assoc(key)
    val = self[key]
    val.nil? ? nil : [key, val]
  end

  def rassoc(val)
    each do |k, v|
      return [k, v] if v == val
    end
    nil
  end

  def key(val)
    each do |k, v|
      return k if v == val
    end
    nil
  end

  def replace(hash)
    clear
    hash.each do |k, v|
      self[k] = v
    end
    self
  end

  def update(hash, &block)
    hash.each do |k, v|
      if block && key?(k)
        self[k] = block.call(k, self[k], v)
      else
        self[k] = v
      end
    end
    self
  end
  alias merge! update

  def select(&block)
    return to_enum(:select) unless block
    to_h.select(&block)
  end
  alias filter select

  def reject(&block)
    return to_enum(:reject) unless block
    to_h.reject(&block)
  end

  def slice(*keys)
    h = {}
    keys.each do |k|
      v = self[k]
      h[k] = v unless v.nil?
    end
    h
  end

  alias include? key?
  alias has_key? key?
  alias member? key?
  alias length size

  def freeze
    raise TypeError, "cannot freeze ENV"
  end
end
