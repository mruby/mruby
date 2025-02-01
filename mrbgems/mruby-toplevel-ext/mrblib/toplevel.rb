def self.include(*modules)
  Object.include(*modules)
end

class <<self
  private

  def private(*methods)
    Object.instance_eval do
      private(*methods)
    end
  end
  def protected(*methods)
    Object.instance_eval do
      protected(*methods)
    end
  end
  def public(*methods)
    Object.instance_eval do
      public(*methods)
    end
  end
end
