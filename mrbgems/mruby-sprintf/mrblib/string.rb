class String
  #
  # call-seq:
  #   str % arg   -> new_str
  #   str % args  -> new_str
  #
  # Format - Uses str as a format specification, and returns the result
  # of applying it to arg. If the format specification contains more than
  # one substitution, then arg must be an Array or Hash containing the values
  # to be substituted. See sprintf for details of the format string.
  #
  #   "%05d" % 123                              #=> "00123"
  #   "%-5s: %016x" % [ "ID", self.object_id ]  #=> "ID   : 00002b054ec93168"
  #   "foo = %{foo}" % { :foo => 'bar' }        #=> "foo = bar"
  #   "%{foo}f" % { :foo => 1 }                 #=> "1f"
  #
  def %(args)
    if args.is_a? Array
      sprintf(self, *args)
    else
      sprintf(self, args)
    end
  end
end
