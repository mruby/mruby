module MRuby
  # An Array of -D defines that remembers, for every entry it holds, the file
  # and line that wrote it. The build and its compilers keep their define
  # lists in one of these so the build can say where each define came from:
  # a build_config line, a gem's mrbgem.rake, a toolchain file. An add made
  # from inside lib/mruby (`enable_debug`) is charged to the first frame
  # outside of it, the config line that asked, with the asking method named.
  #
  # Origins are plain strings so the list survives the Marshal deep clone
  # `Command#clone` copies gem compilers with.
  class DefineList < Array
    LIB_DIR = File.expand_path("../..", __dir__)

    attr_reader :origins

    def initialize(*args)
      super
      @origins = {}
    end

    def initialize_copy(other)
      super
      @origins = {}
      other.origins.each {|name, list| @origins[name] = list.dup}
    end

    def <<(value)
      note(value)
      super
    end

    def push(*values)
      note(values)
      super
    end
    alias append push

    def unshift(*values)
      note(values)
      super
    end
    alias prepend unshift

    def concat(*arrays)
      note(arrays)
      super
    end

    def insert(index, *values)
      note(values)
      super
    end

    # The writer's path, for `list = other` and `list += more` alike: an
    # incoming DefineList (a `.dup` of another) keeps the origins it carries,
    # anything else becomes one, entries the old list already knew keep their
    # origins and the rest are charged to the assignment site.
    def self.assigned(new_list, old_list)
      return new_list if new_list.is_a?(DefineList)

      origin = caller_origin
      old_origins = old_list.is_a?(DefineList) ? old_list.origins : {}
      list = DefineList.new
      list.replace(Array(new_list))
      list.flatten.each do |value|
        define = value.to_s
        if old_origins.key?(define)
          list.origins[define] = old_origins[define].dup
        elsif origin
          (list.origins[define] ||= []) << origin
        end
      end
      list
    end

    # A define may carry a value, as `FOO=1` does; the name is what one -D
    # overrides another by.
    def self.define_name(value)
      value.to_s.split("=", 2).first
    end

    # The first frame outside lib/mruby, so that the reported line is the one
    # a person wrote. When the frame right inside is not a writer method, its
    # name says which switch did the writing (`via enable_debug`).
    def self.caller_origin
      locs = (caller_locations(1, 25) || []).reject do |loc|
        File.expand_path(loc.path) == __FILE__
      end
      index = locs.index {|loc| !File.expand_path(loc.path).start_with?("#{LIB_DIR}/")}
      return nil unless index

      origin = "#{display_path(locs[index].path)}:#{locs[index].lineno}"
      via = index > 0 ? locs[index - 1].base_label : nil
      via = nil if via.nil? || via.end_with?("=")
      via ? "#{origin} (via #{via})" : origin
    end

    def self.display_path(path)
      path = File.expand_path(path)
      root = "#{MRUBY_ROOT}/"
      path.start_with?(root) ? path[root.size..] : path
    end

    private

    # Keyed by the whole define, value and all, so that `FOO=0` and a `FOO=1`
    # written elsewhere each answer with their own writer.
    def note(values)
      origin = DefineList.caller_origin
      return unless origin
      Array(values).flatten.each do |value|
        (@origins[value.to_s] ||= []) << origin
      end
    end
  end
end
