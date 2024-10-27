class Symbol
  def to_proc
    mid = self
    ->(obj,*args,**opts,&block) do
      obj.__send__(mid, *args, **opts, &block)
    end
  end
end
