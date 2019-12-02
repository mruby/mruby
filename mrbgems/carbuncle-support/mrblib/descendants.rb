class Class
  def descendants
    descendants = []
    ObjectSpace.each_object(singleton_class) do |k|
      next if k.singleton_class?

      descendants.unshift k unless k == self
    end
    descendants
  end
end
