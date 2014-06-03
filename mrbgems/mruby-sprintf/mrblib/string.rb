class String
  def %(args)
    if args.is_a? Hash
      sprintf(self, args)
    else
      sprintf(self, *args)
    end
  end
end
