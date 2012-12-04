# --*-- encoding: utf-8 --*--

class OptParser
  def initialize
    @opts = {}
    @vals = {}
  end

  def add(key, value_key, with_value = true)
    @opts[key] = value_key
    @vals[key] = with_value
  end

  def parse(argv)
    key = nil
    values = {}
    argv_ = []
    argv_mode = false

    if argv.size == 0
      return values
    end

    argv.flatten.each do |token|
      if token == "--"
        argv_mode = true
      elsif argv_mode
        argv_ << token
      elsif token[0] == '-' and @opts.keys.include?(token)
        if key
          raise MissingArgument.new("missing argument: #{key}")
        end
        if argv_.size > 0
          raise InvalidOption.new("invalid parameter: #{token}")
        end

        flag = @opts[token]
        unless values[flag]
          values[flag] = []
        end

        key = @vals[token] ? token : nil
      elsif token[0] == '-'
        raise InvalidOption.new("invalid option: #{token}")
      else
        flag = @opts[key]
        if key and values[flag]
          values[flag] << token
        else
          argv_ << token
        end
        key = nil
      end
    end

    if key
      raise MissingArgument.new("missing argument: #{key}")
    end

    values.each do |k,v|
      if v == []
        values[k] = true
      elsif v.size == 1
        values[k] = v.first
      end
    end

    argv_ = nil  if argv_.empty?

    [values, argv_]
  end

  class InvalidOption < StandardError
  end
  class MissingArgument < StandardError
  end
end

if __FILE__ == $0
  $fail = 0

  def assert(msg, obj1, obj2)
    if obj1 == obj2
      puts "Success: #{msg}"
    else
      puts "Failed: #{msg}"
      print "  obj1: "; p obj1
      print "  obj2: "; p obj2
      $fail += 1
    end
  end

  opt = OptParser.new
  opt.add("-a", :a)
  assert("option parse test (simple)", opt.parse(["-a", "b"]), [{:a => "b"}, nil])

  e = nil
  begin
    opt.parse(["-a"])
  rescue => e
  end
  assert("option parse error test (missing argument)", e.class, OptParser::MissingArgument)

  e = nil
  begin
    opt.parse(["-i"])
  rescue => e
  end
  assert("option parse error test (invalid option)", e.class, OptParser::InvalidOption)

  opt.add("-b", :b, false)
  assert("option parse test (many - 1)", opt.parse(["-a", "b", "-b"]), [{:a => "b", :b => true}, nil])
  assert("option parse test (many - 2)", opt.parse(["-a", "b", "-a", "c"]), [{:a => ["b", "c"]}, nil])

  e = nil
  begin
    p opt.parse(["-a", "b", "c", "-b"])
  rescue => e
  end
  assert("argument position invalid", e.class, OptParser::InvalidOption)

  if $fail > 0
    puts "-" * 60
    puts "test failed."
    exit 2
  end
end

