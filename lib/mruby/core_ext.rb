autoload :Pathname, 'pathname'

class Object
  class << self
    def attr_block(*syms)
      syms.flatten.each do |sym|
        class_eval "def #{sym}(&block);block.call(@#{sym}) if block_given?;@#{sym};end"
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

  def remove_leading_parents
    Pathname.new(".#{Pathname.new("/#{self}").cleanpath}").cleanpath.to_s
  end

  def replace_prefix_by(dirmap)
    [self].replace_prefix_by(dirmap)[0]
  end
end

class Array
  # Replace the prefix of each string that is a file path that contains in its own array.
  #
  # dirmap is a hash whose elements are `{ "path/to/old-prefix" => "path/to/new-prefix", ... }`.
  # If it does not match any element of dirmap, the file path is not replaced.
  def replace_prefix_by(dirmap)
    dirmap = dirmap.map { |older, newer| [File.join(older, "/"), File.join(newer, "/")] }
    dirmap.sort!
    dirmap.reverse!
    self.flatten.map do |e|
      map = dirmap.find { |older, newer| e.start_with?(older) }
      e = e.sub(map[0], map[1]) if map
      e
    end
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

# A file task like `file`, for a source that is generated: the block writes
# the text to the IO it is given. The file is written only when the text
# differs from what is there, so the object built from it stays when a
# newer `mrbc` makes the same bytecode.
#
# Rake holds an output out of date when anything behind its prerequisites
# is newer, so a source with `mrbc` among its prerequisites would take its
# object with it whether the text changed or not. The source task has no
# prerequisites of its own: it runs every time and invokes the stamp
# beside it, which has the prerequisites and holds the time of the last
# generation.
def generated_file(name, prerequisites, &block)
  stamp = file "#{name}.stamp" => prerequisites do |t|
    mkdir_p File.dirname(name)
    fresh = "#{name}.tmp"
    File.open(fresh, "w", &block)
    if File.exist?(name) && FileUtils.identical?(fresh, name)
      rm_f fresh
    else
      mv fresh, name
    end
    touch t.name
  end
  stamp.define_singleton_method(:needed?) { super() || !File.exist?(name) }
  source = file name do
    stamp.invoke
  end
  source.define_singleton_method(:needed?) { true }
  source
end
