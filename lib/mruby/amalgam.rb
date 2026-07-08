module MRuby
  class Amalgam
    # Top-level headers to include (internal dependencies are inlined recursively)
    # mruby.h is the main header - it includes value.h, gc.h, version.h with proper macro order
    # Note: internal.h is NOT included here - it goes in mruby.c
    HEADER_ORDER = %w[
      mruby.h
      mruby/array.h
      mruby/string.h
      mruby/hash.h
      mruby/class.h
      mruby/proc.h
      mruby/range.h
      mruby/variable.h
      mruby/numeric.h
      mruby/error.h
      mruby/data.h
      mruby/istruct.h
      mruby/mempool.h
      mruby/debug.h
      mruby/dump.h
      mruby/irep.h
      mruby/opcode.h
      mruby/re.h
      mruby/throw.h
      mruby/khash.h
      mruby/endian.h
      mruby/presym.h
      mruby/compile.h
    ].freeze

    # Boxing headers are conditionally included
    BOXING_HEADERS = %w[
      mruby/boxing_no.h
      mruby/boxing_word.h
      mruby/boxing_nan.h
    ].freeze

    # Core sources in recommended order
    CORE_SOURCE_ORDER = %w[
      allocf.c
      readnum.c
      readint.c
      fp_uscale.c
      state.c
      symbol.c
      class.c
      object.c
      gc.c
      mempool.c
      variable.c
      array.c
      hash.c
      string.c
      range.c
      numeric.c
      numops.c
      proc.c
      kernel.c
      enum.c
      error.c
      backtrace.c
      vm.c
      load.c
      dump.c
      cdump.c
      codedump.c
      print.c
      debug.c
      etc.c
      version.c
      init.c
    ].freeze

    def initialize(build)
      @build = build
      @processed_guards = {}
      @processed_headers = []  # Track header paths for include transformation
      @xmacro_cache = {}
      # Pre-collect gem header names for include transformation
      build_gem_header_map
    end

    # Resolve an include to the gem header file providing it. Basename
    # resolution ("io_hal.h", "../include/mrc_ccontext.h") only applies to
    # quoted includes: an angle include of a basename (#include <time.h>)
    # names a system header even when some gem ships a header with the same
    # basename (mruby/time.h).
    def gem_header_path(header, quote_type)
      @gem_header_map[header] ||
        (quote_type == '"' ? @gem_header_basenames[File.basename(header)] : nil)
    end

    # Include dirs a gem contributes from inside its own tree: its include/
    # dir plus any gem-local cc.include_paths, e.g. mruby-compiler's vendored
    # lib/prism/include. Paths outside the gem dir (build dirs, other gems)
    # are not gem-local headers and are left alone.
    def gem_include_dirs(gem)
      gem_root = File.expand_path(gem.dir)
      dirs = ["#{gem.dir}/include"] + gem.cc.include_paths
      dirs.map { |d| File.expand_path(d) }
        .select { |d| d.start_with?("#{gem_root}/") }
        .select { |d| File.directory?(d) }
        .uniq
    end

    # Map every header name a gem source may #include to the file providing
    # it: @gem_header_map keys paths relative to each include dir
    # ("prism/ast.h"), @gem_header_basenames keys bare basenames ("io_hal.h")
    # for quoted includes. *.inc files are included for X-macro tables like
    # mruby-compiler's mrc_presym.inc.
    def build_gem_header_map
      @gem_header_map = {}
      @gem_header_basenames = {}
      library_gems.each do |gem|
        gem_include_dirs(gem).each do |inc|
          Dir.glob("#{inc}/**/*.{h,inc}").each do |path|
            rel_path = path.sub("#{inc}/", "")
            @gem_header_map[rel_path] ||= path
            @gem_header_basenames[File.basename(rel_path)] ||= path
          end
        end
      end
    end

    # Headers without an include guard are X-macro tables (mrc_ops.h,
    # mrc_presym.inc): their expansion depends on macros defined at each
    # include site, so they must be inlined every time, never deduplicated.
    def gem_xmacro_path(header, quote_type)
      path = gem_header_path(header, quote_type)
      return nil unless path
      unless @xmacro_cache.key?(path)
        @xmacro_cache[path] =
          extract_include_guard(File.read(path, mode: "rb")).nil?
      end
      @xmacro_cache[path] ? path : nil
    end

    def generate_header(output_path)
      FileUtils.mkdir_p(File.dirname(output_path))
      _pp "GEN", output_path.relative_path

      File.open(output_path, "w:binary") do |f|
        write_header_preamble(f)
        write_ordered_headers(f)
        # Boxing headers are inlined at their include point in value.h
        # Presym headers are inlined via presym.h -> enable.h -> id.h
        write_gem_headers(f)
        write_header_postamble(f)
      end
    end

    def generate_source(output_path)
      FileUtils.mkdir_p(File.dirname(output_path))
      _pp "GEN", output_path.relative_path

      File.open(output_path, "w:binary") do |f|
        write_source_preamble(f)
        write_internal_headers(f)
        write_core_sources(f)
        write_generated_sources(f)
        write_gem_sources(f)
      end
    end

    private

    def include_dir
      "#{MRUBY_ROOT}/include"
    end

    def build_include_dir
      "#{@build.build_dir}/include"
    end

    def src_dir
      "#{MRUBY_ROOT}/src"
    end

    # Filter out binary gems (they have main() functions)
    def library_gems
      @build.gems.reject { |gem| gem.name.start_with?("mruby-bin-") }
    end

    # ========== Header Generation ==========

    def write_header_preamble(f)
      f.puts <<~PREAMBLE
        /*
        ** mruby amalgamated header
        ** Generated from mruby source files
        **
        ** This file is auto-generated. Do not edit directly.
        */

        #ifndef MRUBY_AMALGAM_H
        #define MRUBY_AMALGAM_H

        #ifdef __cplusplus
        #define __STDC_LIMIT_MACROS
        #define __STDC_CONSTANT_MACROS
        #define __STDC_FORMAT_MACROS
        #endif

        #include <stdarg.h>
        #include <stdint.h>
        #include <stddef.h>
        #include <limits.h>
        #include <string.h>

      PREAMBLE

      # Add build-level defines from gems (e.g., MRB_USE_TASK_SCHEDULER)
      gem_defines = collect_gem_defines
      unless gem_defines.empty?
        f.puts "/* Gem-required defines */"
        gem_defines.each do |d|
          f.puts "#define #{d}"
        end
        f.puts
      end
    end

    # Collect defines added by gems that affect core headers
    def collect_gem_defines
      defines = []
      @build.defines.each do |d|
        # Include defines that affect mrb_state or core functionality
        defines << d if d =~ /^MRB_USE_|^MRB_UTF8_|^HAVE_MRUBY_/
      end
      defines.uniq.sort
    end

    def write_header_postamble(f)
      f.puts <<~POSTAMBLE

        #endif /* MRUBY_AMALGAM_H */
      POSTAMBLE
    end

    def write_ordered_headers(f)
      # Process top-level headers; internal includes are recursively inlined
      HEADER_ORDER.each do |header|
        path = "#{include_dir}/#{header}"
        next unless File.exist?(path)
        write_header_content(f, header, path)
      end
    end

    def write_boxing_headers(f)
      f.puts "\n/* Boxing type selection */"
      f.puts "#if defined(MRB_NAN_BOXING)"
      write_header_content(f, "mruby/boxing_nan.h", "#{include_dir}/mruby/boxing_nan.h")
      f.puts "#elif defined(MRB_WORD_BOXING)"
      write_header_content(f, "mruby/boxing_word.h", "#{include_dir}/mruby/boxing_word.h")
      f.puts "#else"
      write_header_content(f, "mruby/boxing_no.h", "#{include_dir}/mruby/boxing_no.h")
      f.puts "#endif"
    end

    def write_presym_headers(f)
      presym_dir = "#{@build.build_dir}/include/mruby/presym"
      return unless File.directory?(presym_dir)

      %w[id.h table.h].each do |header|
        path = "#{presym_dir}/#{header}"
        next unless File.exist?(path)
        write_header_content(f, "mruby/presym/#{header}", path)
      end
    end

    def write_gem_headers(f)
      write_gem_cc_defines(f)

      library_gems.each do |gem|
        gem_include = "#{gem.dir}/include"
        next unless File.directory?(gem_include)

        paths = Dir.glob("#{gem_include}/**/*.h")
        order_gem_headers(paths, gem_include).each do |path|
          rel_path = path.sub("#{gem_include}/", "")
          # X-macro tables are inlined at each include site instead
          next if gem_xmacro_path(rel_path, '"')
          write_header_content(f, "#{gem.name}: #{rel_path}", path)
          # Also track the relative path for source include transformation
          # (handles includes like #include "io_hal.h")
          @processed_headers << rel_path unless @processed_headers.include?(rel_path)
        end
      end
    end

    # Defines a gem's mrbgem.rake adds to its own cc (e.g. mruby-compiler's
    # MRC_TARGET_MRUBY and PRISM_XALLOCATOR). In a normal build these are
    # command-line flags for that gem's objects only; the amalgam consumer
    # compiles everything in one plain cc invocation, so embed them before
    # the gem headers that need them. #ifndef lets the consumer override.
    def write_gem_cc_defines(f)
      defines = {}
      build_defines = @build.defines.map { |d| d.to_s }
      library_gems.each do |gem|
        (gem.cc.defines.flatten.map { |d| d.to_s } - build_defines).each do |d|
          name, value = d.split("=", 2)
          defines[name] ||= value
        end
      end
      return if defines.empty?

      f.puts "\n/* Gem compiler defines */"
      defines.sort.each do |name, value|
        f.puts "#ifndef #{name}"
        f.puts(value ? "#define #{name} #{value}" : "#define #{name}")
        f.puts "#endif"
      end
    end

    # Order a gem's headers so each header is emitted after the intra-gem
    # headers it includes. Alphabetical order is wrong: mruby-compiler's
    # mrc_ccontext.h sorts before its dependency mrc_common.h, which left the
    # base types defined too late in the amalgam (see issue #6929). Mutual
    # includes (e.g. mrc_ccontext.h <-> mrc_pool.h) form cycles that the real
    # build resolves with include guards; a DFS post-order breaks them stably
    # by skipping headers already on the current path.
    def order_gem_headers(paths, gem_include)
      by_name = {}
      paths.each { |p| by_name[p.sub("#{gem_include}/", "")] = p }

      deps = {}
      by_name.each do |rel, path|
        included = File.read(path, mode: "rb").scan(/^\s*#\s*include\s+"([^"]+)"/).flatten
        deps[rel] = included.select { |h| by_name.key?(h) }.uniq
      end

      ordered = []
      state = {}   # rel => :active (on path) | :done
      visit = lambda do |rel|
        return if state[rel]
        state[rel] = :active
        deps[rel].sort.each { |d| visit.call(d) unless state[d] == :active }
        state[rel] = :done
        ordered << by_name[rel]
      end
      by_name.keys.sort.each { |rel| visit.call(rel) }
      ordered
    end

    def write_header_content(f, name, path)
      return unless File.exist?(path)

      content = File.read(path, mode: "rb")
      guard = extract_include_guard(content)

      # Skip if already processed
      if guard && @processed_guards[guard]
        f.puts "/* #{name} - already included */"
        return
      end
      @processed_guards[guard] = true if guard
      @processed_headers << name  # Track header path for include transformation

      f.puts "\n/* ======== #{name} ======== */"
      content = strip_include_guard(content, guard) if guard
      content = transform_includes(content, inline: true)
      f.puts content
    end

    # ========== Source Generation ==========

    def write_source_preamble(f)
      f.puts <<~PREAMBLE
        /*
        ** mruby amalgamated source
        ** Generated from mruby source files
        **
        ** This file is auto-generated. Do not edit directly.
        */

        #include "mruby.h"

      PREAMBLE
    end

    def write_internal_headers(f)
      f.puts "/* ======== Internal headers ======== */"

      # Forward declarations needed for amalgamation
      # (functions called before defined due to source file ordering)
      # Note: mrb_irep_catch_handler_table is static inline in internal.h, no forward decl needed
      f.puts <<~FORWARD
        /* Forward declarations for amalgamation */
        static void mrb_irep_free(mrb_state *mrb, mrb_irep *irep);
        static mrb_value mrb_class_find_path(mrb_state *mrb, struct RClass *c);
        static void mrb_method_added(mrb_state *mrb, struct RClass *c, mrb_sym mid);
        static void mrb_proc_copy(mrb_state *mrb, struct RProc *a, const struct RProc *b);
        static size_t mrb_gc_mark_range(mrb_state *mrb, struct RRange *r);
        static void mrb_ary_decref(mrb_state *mrb, mrb_shared_array *shared);
        static mrb_int mrb_proc_arity(const struct RProc *p);
      FORWARD

      # internal.h
      internal_path = "#{include_dir}/mruby/internal.h"
      if File.exist?(internal_path)
        content = File.read(internal_path, mode: "rb")
        content = strip_include_guard(content, extract_include_guard(content))
        content = transform_source_includes(content)
        f.puts "\n/* mruby/internal.h */"
        f.puts content
      end

      # presym/table.h (generated, needed by symbol.c)
      table_path = "#{build_include_dir}/mruby/presym/table.h"
      if File.exist?(table_path)
        content = File.read(table_path, mode: "rb")
        content = transform_source_includes(content)
        f.puts "\n/* mruby/presym/table.h */"
        f.puts content
      end

      # value_array.h (internal src header)
      value_array_path = "#{src_dir}/value_array.h"
      if File.exist?(value_array_path)
        content = File.read(value_array_path, mode: "rb")
        content = strip_include_guard(content, extract_include_guard(content))
        content = transform_source_includes(content)
        f.puts "\n/* src/value_array.h */"
        f.puts content
      end
    end

    def write_core_sources(f)
      f.puts "\n/* ======== Core sources ======== */"

      CORE_SOURCE_ORDER.each do |source|
        path = "#{src_dir}/#{source}"
        next unless File.exist?(path)
        write_source_content(f, "src/#{source}", path)
      end

      # Clear potentially conflicting macros from core sources
      write_macro_cleanup(f, "core")
    end

    def write_generated_sources(f)
      # mrblib.c - compiled Ruby stdlib
      mrblib_path = "#{@build.build_dir}/mrblib/mrblib.c"
      if File.exist?(mrblib_path)
        write_source_content(f, "mrblib.c", mrblib_path)
      end
    end

    # Macros that may conflict between source files in amalgamation
    # These are #undef-ed after each source file within a gem
    CONFLICTING_MACROS = %w[
      mrb_stat
      mrb_lstat
      mrb_fstat
      lesser
      greater
      CASE
      NEXT
      JUMP
      CALL
      node_type
      push
      pop
      peek
      NUMERIC_SHIFT_WIDTH_MAX
    ].freeze

    # Source dirs of libraries vendored inside a gem, recognized by the gem
    # adding a gem-local include path next to a src/ dir (mruby-compiler's
    # lib/prism/include -> lib/prism/src). Their objects are linked into
    # libmruby in a normal build, so the amalgam must carry their sources.
    def gem_vendored_source_dirs(gem)
      gem_include_dirs(gem)
        .reject { |d| d == File.expand_path("#{gem.dir}/include") }
        .map { |d| File.expand_path("#{d}/../src") }
        .select { |d| File.directory?(d) }
        .uniq
    end

    # File-local names defined in a C source: static functions and tables,
    # plus struct/union/enum tags defined at file scope. mruby style puts
    # the name at the start of the line after the type:
    #   static mrb_value
    #   ary_pop(mrb_state *mrb, ...)
    # so scan both same-line and next-line forms.
    def collect_static_syms(paths)
      syms = {}
      paths.each do |path|
        content = File.read(path, mode: "rb")
        content.scan(/^static\s+(?:const\s+)?[\w\s\*]*?\b([a-z_][a-z0-9_]*)\s*[(\[=]/) do |(name)|
          syms[name] = true
        end
        content.scan(/^static\s+(?:const\s+)?[\w\s\*]*?\n([a-z_][a-z0-9_]*)\s*\(/) do |(name)|
          syms[name] = true
        end
        content.scan(/^(?:typedef\s+)?(?:struct|union|enum)\s+([a-z_][a-z0-9_]*)\s*\{/) do |(name)|
          syms[name] = true
        end
      end
      syms
    end

    def seen_static_syms
      @seen_static_syms ||= collect_static_syms(
        CORE_SOURCE_ORDER.map { |s| "#{src_dir}/#{s}" }.select { |p| File.exist?(p) })
    end

    # In the single-translation-unit amalgam, a gem's static helpers can
    # collide with same-named static symbols in core sources or earlier gems
    # (mruby-compiler's cdump.c shares its lineage with src/cdump.c; prism
    # and mruby-regexp both define a `peek`). Rename the colliding ones for
    # the span of the gem's sources with #define, which rewrites
    # definitions and uses consistently.
    def write_static_renames(f, gem, sources)
      gem_syms = collect_static_syms(sources)
      collisions = gem_syms.keys.select { |s| seen_static_syms[s] }
      seen_static_syms.merge!(gem_syms)
      return [] if collisions.empty?

      f.puts "\n/* Rename #{gem.name} static symbols colliding with earlier ones */"
      collisions.sort.each do |sym|
        f.puts "#define #{sym} #{gem.name.tr('-', '_')}_amalg_#{sym}"
      end
      collisions
    end

    def write_static_rename_cleanup(f, collisions)
      return if collisions.empty?
      f.puts
      collisions.sort.each { |sym| f.puts "#undef #{sym}" }
    end

    # Platform sources from the first matching ports/<name>/ dir, same
    # fallback chain the normal build uses (see Gem::Specification#setup).
    def gem_port_source_dirs(gem)
      @build.effective_ports.each do |port|
        port_dir = "#{gem.dir}/ports/#{port}"
        return [port_dir] if File.directory?(port_dir)
      end
      []
    end

    def write_gem_sources(f)
      f.puts "\n/* ======== Gem sources ======== */"

      library_gems.each do |gem|
        # Some gems use 'core/' instead of 'src/' (mruby-compiler, mruby-bigint)
        source_dirs = ["#{gem.dir}/src", "#{gem.dir}/core"].select { |d| File.directory?(d) }
        source_dirs += gem_vendored_source_dirs(gem)
        source_dirs += gem_port_source_dirs(gem)

        # Include C sources if the gem has any
        unless source_dirs.empty?
          sources = source_dirs.flat_map { |d| Dir.glob("#{d}/**/*.c") }.sort
          renamed = write_static_renames(f, gem, sources)
          sources.each_with_index do |path, idx|
            rel_path = path.sub("#{gem.dir}/", "")
            write_source_content(f, "#{gem.name}: #{rel_path}", path)
            # Clear macros between source files to avoid conflicts
            # (e.g., mrb_stat macro in file.c vs function in file_test.c)
            write_macro_cleanup(f, "#{gem.name}/#{File.basename(path)}") if idx < sources.size - 1
          end
          write_static_rename_cleanup(f, renamed)
        end

        # Gem's compiled mrblib (Ruby-only gems like mruby-enum-ext have this)
        gem_mrblib = "#{gem.build_dir}/gem_mrblib.c"
        if File.exist?(gem_mrblib)
          write_source_content(f, "#{gem.name}: gem_mrblib.c", gem_mrblib)
        end

        # Gem's init functions (GENERATED_TMP_mrb_*_gem_init/final)
        # Required for both C and Ruby-only gems
        gem_init = "#{gem.build_dir}/gem_init.c"
        if File.exist?(gem_init)
          write_source_content(f, "#{gem.name}: gem_init.c", gem_init)
        end

        # Clear potentially conflicting macros after each gem
        write_macro_cleanup(f, gem.name)
      end

      # gem_init.c - gem registration
      gem_init_path = "#{@build.build_dir}/mrbgems/gem_init.c"
      if File.exist?(gem_init_path)
        write_source_content(f, "gem_init.c", gem_init_path)
      end
    end

    def write_source_content(f, name, path)
      return unless File.exist?(path)

      content = File.read(path, mode: "rb")
      # For source files, comment out all mruby includes (they're in the header)
      # and inline local includes (like .cstub files)
      source_dir = File.dirname(path)
      content = transform_source_includes(content, source_dir)
      # Drop #line markers (prism's generated sources carry them): they would
      # make compiler diagnostics point into .erb templates instead of the
      # amalgamated file.
      content = content.gsub(/^\s*#\s*line\b.*\n/, "")

      f.puts "\n/* ======== #{name} ======== */"
      f.puts content
    end

    def write_macro_cleanup(f, gem_name)
      f.puts "\n/* Cleanup macros from #{gem_name} to avoid conflicts */"
      CONFLICTING_MACROS.each do |macro|
        f.puts "#ifdef #{macro}"
        f.puts "#undef #{macro}"
        f.puts "#endif"
      end
    end

    # X-macro pattern headers that must be inlined every time they're included
    # (not commented out) because they expand differently based on macro definitions
    XMACRO_HEADERS = %w[
      mruby/ops.h
    ].freeze

    def transform_source_includes(content, source_dir = nil)
      content.gsub(/^(\s*)(#\s*include\s+([<"])([^>"]+)[>"])/m) do |match|
        prefix = $1
        include_stmt = $2
        quote_type = $3  # < or "
        header = $4

        # X-macro headers must be inlined every time (not commented out)
        # because they expand differently based on surrounding macro definitions
        if XMACRO_HEADERS.include?(header) || gem_xmacro_path(header, quote_type)
          xmacro_path = XMACRO_HEADERS.include?(header) ?
                        "#{include_dir}/#{header}" : gem_xmacro_path(header, quote_type)
          if File.exist?(xmacro_path)
            xmacro_content = File.read(xmacro_path, mode: "rb")
            "#{prefix}/* Inlined X-macro: #{header} */\n#{xmacro_content}"
          else
            match
          end
        # Comment out all mruby-related includes and any header already in amalgam
        elsif mruby_header?(header) || header == "mruby.h" ||
           @processed_headers.include?(header) || gem_header_path(header, quote_type)
          "#{prefix}// #{include_stmt} - in amalgam header"
        elsif source_dir && quote_type == '"' && !header.include?("/")
          # Check for local includes like "known_errors_def.cstub"
          local_path = "#{source_dir}/#{header}"
          if File.exist?(local_path)
            local_content = File.read(local_path, mode: "rb")
            "#{prefix}/* Inlined: #{header} */\n#{local_content}"
          else
            match
          end
        else
          match
        end
      end
    end

    # ========== Content Transformation ==========

    # A C comment that ends at its own */ — unlike /\*.*?\*/m it cannot
    # stretch across code under backtracking, which would let a guard-less
    # header with a #ifndef further down be misdetected as guarded.
    C_COMMENT = /\/\*(?:[^*]|\*(?!\/))*\*\//

    def extract_include_guard(content)
      # Match #ifndef GUARD_NAME at start of file (after comments)
      if content =~ /\A(?:#{C_COMMENT}\s*|\/\/[^\n]*\n)*\s*#ifndef\s+(\w+)\s*\n\s*#define\s+\1/
        $1
      end
    end

    def strip_include_guard(content, guard)
      return content unless guard

      # Remove opening #ifndef GUARD but KEEP #define GUARD (needed for #ifdef checks)
      content = content.sub(/\A((?:#{C_COMMENT}\s*|\/\/[^\n]*\n)*\s*)#ifndef\s+#{guard}\s*\n/, '\1')

      # Remove closing #endif (any comment is ok, guard name may not match exactly)
      content = content.sub(/\n#endif\s*(?:\/\*[^*]*\*\/|\/\/[^\n]*)?\s*\z/m, "\n")

      content
    end

    def transform_includes(content, inline: false)
      # Match both "#include" and "# include" (preprocessor allows spaces)
      # Only match includes at the start of a line (real preprocessor directives)
      content.gsub(/^(\s*)(#\s*include\s+([<"])([^>"]+)[>"])/m) do |match|
        prefix = $1
        include_stmt = $2
        quote_type = $3
        header = $4

        if (xmacro_path = gem_xmacro_path(header, quote_type))
          # X-macro tables expand differently at each include site
          "#{prefix}/* Inlined X-macro: #{header} */\n" +
            File.read(xmacro_path, mode: "rb")
        elsif already_included?(header)
          # Keep original whitespace, comment out the include
          "#{prefix}// #{include_stmt} - in amalgam"
        elsif inline && mruby_header?(header)
          # Recursively inline this header
          "#{prefix}#{inline_header(header)}"
        elsif (gem_path = gem_header_path(header, quote_type))
          # Gem-local header (e.g. mruby-compiler's mrc_common.h or its
          # vendored prism.h). Inline it at the first include site; later
          # occurrences are commented out via already_included?.
          if inline
            "#{prefix}#{inline_gem_header(header, gem_path)}"
          else
            "#{prefix}// #{include_stmt} - in amalgam (gem header)"
          end
        else
          match
        end
      end
    end

    def already_included?(header)
      # Check if header was already processed
      # Only use end_with? matching for mruby headers to avoid matching
      # system headers like <time.h> against mruby/time.h
      if mruby_header?(header)
        @processed_headers.any? { |h| h == header || h.end_with?("/#{header}") }
      else
        @processed_headers.include?(header)
      end
    end

    def mruby_header?(header)
      # Check if this is an mruby header that should be inlined/transformed
      header == "mruby.h" || header.start_with?("mruby/") ||
        %w[mrbconf.h boxing_nan.h boxing_word.h boxing_no.h common.h object.h value_array.h].include?(header)
    end

    def inline_header(header)
      # Find the full path for this header
      if header == "mruby.h" || header == "mrbconf.h" || header.start_with?("mruby/")
        full_header = header
        # Check both source include dir and build include dir (for generated headers)
        path = "#{include_dir}/#{header}"
        path = "#{build_include_dir}/#{header}" unless File.exist?(path)
      else
        # Relative includes like "boxing_word.h" -> "mruby/boxing_word.h"
        full_header = "mruby/#{header}"
        path = "#{include_dir}/#{full_header}"
        path = "#{build_include_dir}/#{full_header}" unless File.exist?(path)
      end

      return "/* #{header} - not found */" unless File.exist?(path)

      # Mark as processed to avoid infinite recursion
      @processed_headers << header
      @processed_headers << full_header

      content = File.read(path, mode: "rb")
      guard = extract_include_guard(content)

      if guard && @processed_guards[guard]
        return "/* #{header} - already included */"
      end
      @processed_guards[guard] = true if guard

      content = strip_include_guard(content, guard) if guard
      content = transform_includes(content, inline: true)

      "\n/* ======== #{full_header} (inlined) ======== */\n#{content}"
    end

    # Inline a gem-provided header at its include site, recursively
    # transforming its own includes.
    def inline_gem_header(header, path)
      content = File.read(path, mode: "rb")
      guard = extract_include_guard(content)

      if guard && @processed_guards[guard]
        return "/* #{header} - already included */"
      end
      @processed_guards[guard] = true if guard
      @processed_headers << header unless @processed_headers.include?(header)

      content = strip_include_guard(content, guard) if guard
      content = transform_includes(content, inline: true)

      "\n/* ======== #{header} (inlined) ======== */\n#{content}"
    end
  end
end
