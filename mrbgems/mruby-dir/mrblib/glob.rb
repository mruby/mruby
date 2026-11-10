class Dir
  class << self
    #
    # call-seq:
    #   Dir.glob(pattern, flags = 0, base: nil, sort: true)                 -> array
    #   Dir.glob(pattern, flags = 0, base: nil, sort: true) { |name| block } -> nil
    #
    # Returns the names of the files that match pattern, a String (or an
    # object with to_path, such as a Pathname) or an Array of them. A
    # pattern is matched name by name, as File.fnmatch matches it under
    # File::FNM_PATHNAME:
    #
    # - `*` matches any name, or any part of one, `?` any one character,
    #   and `[set]` one character of the set;
    # - `**/` matches any number of directories, none of them a symbolic
    #   link;
    # - `{a,b}` matches either pattern;
    # - a pattern that ends in `/` matches directories only, and their
    #   names end in `/`.
    #
    # A wildcard does not match a name with a leading period unless
    # File::FNM_DOTMATCH is given, and `..` is never matched by one.
    # Names are relative to base when it is given, and to the current
    # directory otherwise. With sort: true (the default) the names of each
    # directory come in sorted order.
    #
    #   Dir.glob("*.rb")                   #=> ["main.rb"]
    #   Dir.glob("**/*.rb")                #=> ["lib/song.rb", "main.rb"]
    #   Dir.glob("*", base: "lib")         #=> ["song.rb"]
    #   Dir.glob(["*.rb", "*.txt"])        #=> ["main.rb", "notes.txt"]
    #
    def glob(pattern, flags = 0, base: nil, sort: true, &block)
      patterns = pattern.is_a?(Array) ? pattern : [pattern]
      unless base.nil?
        base = __glob_path(base)
        raise TypeError, "#{base.class} cannot be converted to String" unless base.is_a?(String)
      end
      g = {
        base: base, sort: sort, found: [],
        flags: flags,
        mflags: flags & (File::FNM_NOESCAPE | File::FNM_DOTMATCH | File::FNM_CASEFOLD),
        dotmatch: (flags & File::FNM_DOTMATCH) != 0,
      }
      i = 0
      while i < patterns.size
        pat = __glob_path(patterns[i])
        raise TypeError, "#{pat.class} cannot be converted to String" unless pat.is_a?(String)
        __glob_pattern(g, pat)
        i += 1
      end
      found = g[:found]
      return found unless block
      found.each(&block)
      nil
    end

    #
    # call-seq:
    #   Dir[pattern, ..., base: nil, sort: true]  -> array
    #
    # Dir.glob of the patterns, with no flags.
    #
    #   Dir["*.rb", "*.txt"]   #=> ["main.rb", "notes.txt"]
    #
    def [](*patterns, base: nil, sort: true)
      glob(patterns, 0, base: base, sort: sort)
    end

    # A String, or what an object with to_path answers
    def __glob_path(obj)
      return obj if obj.is_a?(String) || !obj.respond_to?(:to_path)
      obj.to_path
    end

    # A pattern that starts with a brace is spelt out whole; any other
    # brace is spelt out where the walk reaches it.
    def __glob_pattern(g, pat)
      if pat[0] == "{"
        alternatives = __glob_braces(pat, g[:flags], [])
        i = 0
        while i < alternatives.size
          __glob_pattern(g, alternatives[i])
          i += 1
        end
        return
      end
      root = nil
      rest = pat
      if pat[0] == "/"
        root = "/"
        rest = pat[1, pat.size]
      end
      segs = __glob_compile(rest, g[:flags])
      return if root.nil? && segs.empty?
      g[:root] = root
      __glob_walk(g, segs, root || "", [0], false, false)
    end

    # Spells out the first outermost {a,b} of str, and the rest of the
    # braces in each pattern that gives, onto out. A { without its } leaves
    # no pattern.
    def __glob_braces(str, flags, out)
      escape = (flags & File::FNM_NOESCAPE) == 0
      lbrace = rbrace = nil
      nest = 0
      i = 0
      n = str.size
      while i < n
        c = str[i]
        if c == "{"
          lbrace = i if nest == 0
          nest += 1
        elsif c == "}" && lbrace
          nest -= 1
          if nest == 0
            rbrace = i
            break
          end
        elsif c == "\\" && escape
          i += 1
        end
        i += 1
      end

      unless lbrace
        out << str
        return out
      end
      return out unless rbrace

      prefix = str[0, lbrace]
      suffix = str[rbrace + 1, n]
      i = lbrace
      while i < rbrace
        i += 1
        start = i
        nest = 0
        while i < rbrace && !(str[i] == "," && nest == 0)
          c = str[i]
          if c == "{"
            nest += 1
          elsif c == "}"
            nest -= 1
          elsif c == "\\" && escape
            i += 1
            break if i == rbrace
          end
          i += 1
        end
        __glob_braces(prefix + str[start, i - start] + suffix, flags, out)
      end
      out
    end

    # A relative pattern as a list of names, each [kind, name, unescaped
    # name]. kind is :plain, :magic (a wildcard), :brace (a {a,b}, which
    # may span a "/"), or :recursive (a "**/"); a pattern that ends in "/"
    # ends with [:dir].
    def __glob_compile(pat, flags)
      escape = (flags & File::FNM_NOESCAPE) == 0
      segs = []
      i = 0
      n = pat.size
      while i < n
        # a "**/" (or several) matches any number of directories
        if pat[i, 3] == "**/"
          while pat[i, 3] == "**/"
            i += 3
            i += 1 while i < n && pat[i] == "/"
          end
          segs << [:recursive]
          segs << [:dir] if i == n
          next
        end
        # the name up to the next "/" outside a [set] or a {a,b}
        start = i
        kind = :plain
        open = false
        while i < n
          c = pat[i]
          if c == "\\" && escape
            i += 1
          elsif c == "[" || c == "{"
            open = true
            kind = c == "{" ? :brace : (kind == :brace ? :brace : :magic)
          elsif c == "]" || c == "}"
            open = false
          elsif c == "/"
            break unless open
          elsif c == "*" || c == "?"
            kind = :magic unless kind == :brace
          end
          i += 1
        end
        name = pat[start, i - start]
        segs << [kind, name, kind == :plain && escape ? __glob_unescape(name) : name]
        if i < n
          i += 1
          segs << [:dir] if i == n
        end
      end
      segs
    end

    # The pattern again, from position i of segs on
    def __glob_source(segs, i)
      parts = []
      while i < segs.size
        seg = segs[i]
        parts << (seg[0] == :recursive ? "**" : seg[0] == :dir ? "" : seg[1])
        i += 1
      end
      parts.join("/")
    end

    # name without the backslashes that escape its characters
    def __glob_unescape(name)
      return name unless name.include?("\\")
      # String#<< comes with mruby-string-ext, which this gem goes without
      out = []
      i = 0
      n = name.size
      while i < n
        i += 1 if name[i] == "\\" && i + 1 < n
        out << name[i]
        i += 1
      end
      out.join
    end

    # The file that path names: path itself, or path under base
    def __glob_fs(g, path)
      base = g[:root] ? nil : g[:base]
      if base
        path.empty? ? base : File.join(base, path)
      else
        path.empty? ? "." : path
      end
    end

    def __glob_join(g, path, name)
      return name if path.empty?
      return path + name if path == g[:root]
      path + "/" + name
    end

    def __glob_symlink?(fs)
      File.respond_to?(:symlink?) && File.symlink?(fs)
    end

    def __glob_match?(g, seg, name)
      return File.fnmatch(seg[1], name, g[:mflags]) unless seg[0] == :brace
      alternatives = __glob_braces(seg[1], g[:flags], [])
      i = 0
      while i < alternatives.size
        return true if File.fnmatch(alternatives[i], name, g[:mflags])
        i += 1
      end
      false
    end

    # Matches the names at positions in segs against what is under path.
    # known is true when path came from reading its directory, and so is
    # there; below a directory that has been read, "." is not matched
    # again (skipdot).
    def __glob_walk(g, segs, path, positions, known, skipdot)
      match_all = match_dir = magical = plain = recursive = brace = false
      k = 0
      while k < positions.size
        i = positions[k]
        kind = segs[i] ? segs[i][0] : :end
        if kind == :recursive
          recursive = true
          i += 1
          kind = segs[i] ? segs[i][0] : :end
        end
        case kind
        when :end then match_all = true
        when :dir then match_dir = true
        when :magic then magical = true
        when :plain then plain = true
        when :brace
          # matched name by name under a "**/", unless it spans a "/"
          brace = true if !recursive || segs[i][1].include?("/")
        end
        k += 1
      end

      if brace
        alternatives = __glob_braces(__glob_source(segs, positions[0]), g[:flags], [])
        k = 0
        while k < alternatives.size
          __glob_walk(g, __glob_compile(alternatives[k], g[:flags]), path, [0], known, skipdot)
          k += 1
        end
        return
      end

      found = g[:found]
      unless path.empty?
        fs = __glob_fs(g, path)
        # a dangling symbolic link is there, as lstat sees it
        found << path if match_all && (known || File.exist?(fs) || __glob_symlink?(fs))
        found << path + "/" if match_dir && File.directory?(fs)
      end

      if magical || recursive
        names = begin
          Dir.entries(__glob_fs(g, path))
        rescue StandardError
          return
        end
        names.sort! if g[:sort]
        dotmatch = g[:dotmatch]
        n = 0
        while n < names.size
          name = names[n]
          n += 1
          next if name == ".."
          dot = name[0] == "." ? (name == "." ? 2 : 1) : 0
          # "." is never descended into by "**/"
          next if dot == 2 && (skipdot || (recursive && !dotmatch))
          child = __glob_join(g, path, name)
          realdir = nil
          next_positions = []
          k = 0
          while k < positions.size
            i = positions[k]
            k += 1
            seg = segs[i]
            next unless seg
            if seg[0] == :recursive
              if dot < (dotmatch ? 2 : 1)
                if realdir.nil?
                  fs = __glob_fs(g, child)
                  realdir = File.directory?(fs) && !__glob_symlink?(fs)
                end
                next_positions << i if realdir
              end
              i += 1
              seg = segs[i]
              next unless seg
            end
            next if seg[0] == :dir
            next_positions << i + 1 if __glob_match?(g, seg, name)
          end
          __glob_walk(g, segs, child, next_positions, true, true) unless next_positions.empty?
        end
      elsif plain
        # a name without wildcards is looked for, not read for; positions
        # that look for the same name go on together
        taken = []
        k = 0
        while k < positions.size
          i = positions[k]
          seg = segs[i]
          if seg && seg[0] == :plain && !taken[k]
            name = seg[2]
            next_positions = [i + 1]
            m = k + 1
            while m < positions.size
              other = segs[positions[m]]
              if !taken[m] && other && other[0] == :plain && File.fnmatch(other[1], name, g[:mflags])
                next_positions << positions[m] + 1
                taken[m] = true
              end
              m += 1
            end
            __glob_walk(g, segs, __glob_join(g, path, name), next_positions, false, skipdot)
          end
          k += 1
        end
      end
    end
  end
end
