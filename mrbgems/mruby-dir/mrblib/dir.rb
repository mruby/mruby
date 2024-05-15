class Dir
  include Enumerable

  def each(&block)
    return to_enum(:each) unless block
    while s = self.read
      block.call(s)
    end
    self
  end

  def each_child(&block)
    return to_enum(:each_child) unless block
    while s = self.read
      block.call(s) unless s == "." || s == ".."
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

    def children(path)
      a = []
      self.open(path) do |d|
        while s = d.read
          a << s unless s == "." || s == ".."
        end
      end
      a
    end

    def foreach(path, &block)
      return to_enum(:foreach, path) unless block
      self.open(path) do |d|
        d.each(&block)
      end
    end

    def open(path, &block)
      if block
        d = self.new(path)
        begin
          block.call(d)
        ensure
          begin
            d.close
          rescue IOError
          end
        end
      else
        self.new(path)
      end
    end

    def chdir(path, &block)
      if block
        wd = self.getwd
        begin
          self._chdir(path)
          block.call(path)
        ensure
          self._chdir(wd)
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
