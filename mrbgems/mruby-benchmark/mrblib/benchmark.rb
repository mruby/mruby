module Benchmark
  # Timing measurement result
  class Tms
    attr_reader :utime, :stime, :cutime, :cstime, :real
    attr_reader :objects, :memory

    def initialize(utime, stime, cutime, cstime, real, label = nil, objects = nil, memory = nil)
      @utime = utime
      @stime = stime
      @cutime = cutime
      @cstime = cstime
      @real = real
      @label = label
      @objects = objects
      @memory = memory
    end

    def total
      @utime + @stime + @cutime + @cstime
    end

    def to_s
      "%10.6f %10.6f %10.6f (%10.6f)\n" % [@utime, @stime, total, @real]
    end

    def format(format_str)
      str = format_str.dup
      str.gsub!('%u', @utime.to_s)
      str.gsub!('%s', @stime.to_s)
      str.gsub!('%t', total.to_s)
      str.gsub!('%r', @real.to_s)
      str.gsub!('%o', @objects.to_s) if @objects
      str.gsub!('%m', @memory.to_s) if @memory
      str.gsub!('%n', @label.to_s) if @label
      str
    end
  end

  # Report class for formatted benchmark output
  class Report
    def initialize(width = 0)
      @width = width
      @results = []
    end

    def report(label = "")
      tms = Benchmark.measure { yield }
      # Create new Tms with label set
      tms = Benchmark::Tms.new(tms.utime, tms.stime, tms.cutime, tms.cstime,
                                tms.real, label, tms.objects, tms.memory)

      label_str = label.to_s
      if label_str.length < @width
        label_str = label_str + " " * (@width - label_str.length)
      end

      if $stdout
        $stdout.print label_str
        $stdout.print tms.to_s
      end

      @results << tms
      tms
    end

    def results
      @results
    end
  end

  # Measure execution time of a block
  def self.measure(memory: false)
    start_objects = nil
    start_count = nil

    if memory
      if Object.const_defined?(:ObjectSpace)
        start_count = ObjectSpace.count_objects
        start_objects = start_count.values.inject(0) {|sum, n| sum + n }
      end
    end

    t0 = Process.times
    r0 = Benchmark.__clock_ns
    yield
    t1 = Process.times
    r1 = Benchmark.__clock_ns

    objects_allocated = nil
    memory_allocated = nil

    if memory && start_count
      end_count = ObjectSpace.count_objects
      end_objects = end_count.values.inject(0) {|sum, n| sum + n }
      objects_allocated = end_objects - start_objects

      # Estimate memory based on object count
      # Average object overhead in mruby (approximate)
      memory_allocated = objects_allocated * 40
    end

    Tms.new(t1.utime - t0.utime, t1.stime - t0.stime,
            t1.cutime - t0.cutime, t1.cstime - t0.cstime,
            (r1 - r0) / 1e9, nil, objects_allocated, memory_allocated)
  end

  # Return only real time as a float
  def self.realtime
    r0 = __clock_ns
    yield
    (__clock_ns - r0) / 1e9
  end

  # The monotonic clock in integer nanoseconds. Two readings subtracted as
  # Float lose a short interval to rounding where Float is single precision.
  def self.__clock_ns
    Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
  end

  # Formatted benchmark with labeled reports
  def self.bm(label_width = 0)
    report = Report.new(label_width)

    # Print header
    if $stdout
      if label_width > 0
        $stdout.print " " * label_width
      end
      $stdout.puts "      user     system      total        real"
    end

    yield report

    report
  end
end
