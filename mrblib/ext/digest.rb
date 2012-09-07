module Digest
  class Base
    def self.file(path)
      self.new.update(File.open(path).read)
    end
    def file(path)
      self.update(File.open(path).read)
    end
  end

  class HMAC
    def self.digest(data, key, digest)
      self.new(key, digest).update(data).digest
    end
    def self.hexdigest(data, key, digest)
      self.new(key, digest).update(data).hexdigest
    end

    alias << update
    alias to_s hexdigest
  end
end
