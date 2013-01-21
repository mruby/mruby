class Object
  class << self
    def attr_block(*syms)
      syms.flatten.each do |sym|
        class_eval "def #{sym}(&block);block.call(@#{sym}) if block_given?;@#{sym};end"
      end
    end
  end
end

class String
  def relative_path_from(dir)
    Pathname.new(File.expand_path(self)).relative_path_from(Pathname.new(File.expand_path(dir))).to_s
  end
  
  # Compatible with 1.9 on 1.8
  def %(params)
    if params.is_a?(Hash)
      str = self.clone
      params.each do |k, v|
        str.gsub!("%{#{k}}", v)
      end
      str
    else
      if params.is_a?(Array)
        sprintf(self, *params)
      else
        sprintf(self, params)
      end
    end
  end
end

class Symbol
  # Compatible with 1.9 on 1.8
  def to_proc
    proc { |obj, *args| obj.send(self, *args) }
  end
end
