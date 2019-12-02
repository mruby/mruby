class Module
  def delegate(*methods, to:, prefix: false)
    prefix = "#{to}_" if prefix == true
    prefix = '' if prefix == false

    methods.each do |name|
      method_name = "#{prefix}#{name}"
      if /\=$/.match?(name.to_s)
        define_method(method_name) { |value| send(to)&.send(name, value) }
      else
        define_method(method_name) { |*args| send(to)&.send(name, *args) }
      end
    end
  end
end
