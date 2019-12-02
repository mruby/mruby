class Regexp
  def match?(*args)
    match(*args.map(&:to_s)).present?
  end
end
