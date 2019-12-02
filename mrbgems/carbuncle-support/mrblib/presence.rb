class Object
  def blank?
    respond_to?(:empty?) ? !!empty? : !self
  end

  def present?
    !blank?
  end

  def presence
    present? ? self : nil
  end
end
