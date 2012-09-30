class IOError < StandardError; end
class EOFError < StandardError; end

class IO
  def self.open(*args, &block)
    io = self.new(*args)

    return io unless block

    begin
      yield io
    ensure
      begin
        io.close unless io.closed?
      rescue StandardError
        # nothing
      end
    end
  end
end
