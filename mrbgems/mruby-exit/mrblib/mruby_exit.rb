class SystemExit
  ##
  # call-seq:
  #   SystemExit.new(status = true, message = "SystemExit")  -> system_exit
  #   SystemExit.new(message)                                -> system_exit
  #
  # Creates a SystemExit with the exit status given, `true` and `false`
  # meaning success and failure. Without a status, it stands for success.
  #
  #   SystemExit.new.status          #=> 0
  #   SystemExit.new(false).status   #=> 1
  #   SystemExit.new(3, "bye").status #=> 3
  #
  def initialize(*args)
    raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0..2)" if args.size > 2
    status = args[0]
    if status == true
      @status = 0
    elsif status == false
      @status = 1
    elsif status.is_a?(Integer)
      @status = status
    else
      raise ArgumentError, "wrong number of arguments (given 2, expected 0..1)" if args.size == 2
      @status = 0
      return super
    end
    args.size == 2 ? super(args[1]) : super()
  end

  ##
  # call-seq:
  #   system_exit.status  -> integer
  #
  # The exit status.
  #
  def status
    @status
  end

  ##
  # call-seq:
  #   system_exit.success?  -> true or false
  #
  # Whether the exit status means success: it is 0.
  #
  def success?
    @status == 0
  end
end
