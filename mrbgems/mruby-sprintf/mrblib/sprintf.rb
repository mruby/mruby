class String
  def %(args)
    if args.class == Array
      return sprintf(self, *args)
    else
      return sprintf(self, args);
    end
  end
end
