class Module
  # 15.2.2.4.13
  def attr_reader(*names)
    names.each{|name|
      define_method(name){self.instance_variable_get('@'+name.to_s)}
    }
  end
  # 15.2.2.4.14
  def attr_writer(*names)
    names.each{|name|
      aset = (name.to_s+"=").intern
      define_method(aset){|v|self.instance_variable_set('@'+name.to_s,v)}
    }
  end
  # 15.2.2.4.12
  def attr_accessor(*names)
    attr_reader(*names)
    attr_writer(*names)
  end
  # 15.2.2.4.11
  def attr(name)
    attr_reader(name)
  end
end
