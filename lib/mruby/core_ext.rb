autoload :Pathname, 'pathname'

class Object
  class << self
    def attr_block(*syms)
      syms.flatten.each do |sym|
        class_eval "def #{sym}(&block);block.call(@#{sym}) if block_given?;@#{sym};end"
      end
    end
  end

  def fileset_list_under(basedir)
    [self].fileset_list_under(basedir)
  end

  def fileset_make_under(*dirs, prefix: nil)
    [self].fileset_make_under(*dirs, prefix: prefix)
  end
end

class Array
  def fileset_list_under(basedir)
    basedir = Pathname.new(basedir)
    alist = []
    _fileset_traverse_under(basedir, nil, nil, nil) do |path|
      alist << path.to_path
      nil
    end
    alist
  end

  def fileset_make_under(basedir, *dirs, prefix: nil)
    basedir = Pathname.new(basedir)
    prefix = "#{prefix}:" if prefix
    alist = {}
    _fileset_traverse_under(basedir, dirs, prefix, alist) do |path|
      case path
      when nil
        nil
      when Pathname # pathname + string'd-path
        path.read
      else
        raise TypeError, "wrong type #{path.inspect}"
      end
    end
    alist.empty? ? nil : alist
  end

  def _fileset_traverse_under(basedir, dirs, prefix, alist, &block)
    flatten.each do |entry|
      case entry
      when nil
        # do nothing
      when String
        path = basedir + entry
        name = path.to_s.relative_path_under(basedir, *dirs)
        entity = yield path
        unless alist.nil? || entity.nil? || entity.empty?
          key = "#{prefix}#{name}"
          raise "conflict file name - #{key} for #{basedir}" if alist.key?(key)
          alist[key] = entity
        end
      else
        raise TypeError, "expect string only for `terms`, but actual #{entry.inspect}"
      end
    end
  end
end

class String
  def relative_path_from(dir)
    Pathname.new(File.expand_path(self)).relative_path_from(Pathname.new(File.expand_path(dir))).to_s
  end

  def relative_path
    relative_path_from(Dir.pwd)
  end

  def relative_path_under(*dirs)
    dirs.each do |dir|
      return relative_path_from dir if start_with? File.join(dir, "/")
    end

    self
  end

  def remove_leading_parents
    Pathname.new(".#{Pathname.new("/#{self}").cleanpath}").cleanpath.to_s
  end
end

def install_D(src, dst)
  _pp "INSTALL", src.relative_path, dst.relative_path
  rm_f dst
  mkdir_p File.dirname(dst)
  cp src, dst
end

def _pp(cmd, src, tgt=nil, indent: nil)
  return if Rake.application.options.silent

  width = 5
  template = indent ? "%#{width * indent}s %s %s" : "%-#{width}s %s %s"
  puts template % [cmd, src, tgt ? "-> #{tgt}" : nil]
end
