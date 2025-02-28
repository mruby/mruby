class <<self
  private

  def include(*modules)
    Object.include(*modules)
  end
end
