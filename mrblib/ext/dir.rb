if Object.const_defined? :Dir
  class Dir
    def each(&block)
      while s = self.read
        block.call(s)
      end
      self
    end

    alias pos tell
    alias pos= seek

    def self.entries(path)
      a = []
      self.open(path) { |d|
        while s = d.read
	  a << s
	end
      }
      a
    end

    def self.foreach(path, &block)
      if block
        self.open(path).each { |f| block.call(f) }
      else
        self.open(path).each
      end
    end

    def self.open(path, &block)
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

    class << self
      alias exists? exist?
      alias pwd getwd
      alias rmdir delete
      alias unlink delete
    end
  end
end
