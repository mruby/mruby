class Dir
  def each(&block)
    while s = self.read
      block.call(s)
    end
    self
  end

  alias pos tell
  alias pos= seek

  class << self
    def entries(path)
      a = []
      self.open(path) do |d|
        while s = d.read
          a << s
        end
      end
      a
    end

    def foreach(path, &block)
      if block
        self.open(path).each {|f| block.call(f) }
      else
        self.open(path).each
      end
    end

    def open(path, &block)
      if block
        d = self.new(path)
        begin
          block.call(d)
        ensure
          d.close
        end
      else
        self.new(path)
      end
    end

    def chdir(path, &block)
      my = self # workaround for https://github.com/mruby/mruby/issues/1579
      if block
        wd = self.getwd
        begin
          self._chdir(path)
          block.call(path)
        ensure
          my._chdir(wd)
        end
      else
        self._chdir(path)
      end
    end

    alias exists? exist?
    alias pwd getwd
    alias rmdir delete
    alias unlink delete
  end
end
