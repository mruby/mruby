module Digest
  class Base
    def self.file(path)
      self.new.update(File.open(path).read)
    end
    def file(path)
      self.update(File.open(path).read)
    end
  end
end
