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
      readfloat.c
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
      fmt_fp.c
      debug.c
      etc.c
      version.c
      init.c
    ].freeze

    def initialize(build)
      @build = build
      @processed_guards = {}
      @processed_headers = []  # Track header paths for include transformation
      # Pre-collect gem header names for source include transformation
      @gem_header_names = collect_gem_header_names
    end

    def collect_gem_header_names
      names = []
      library_gems.each do |gem|
        gem_include = "#{gem.dir}/include"
        next unless File.directory?(gem_include)
        Dir.glob("#{gem_include}/**/*.h").each do |path|
          rel_path = path.sub("#{gem_include}/", "")
          names << rel_path
          # Also track basename for simple includes like "io_hal.h"
          names << File.basename(rel_path)
        end
      end
      names.uniq
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
      library_gems.each do |gem|
        gem_include = "#{gem.dir}/include"
        next unless File.directory?(gem_include)

        Dir.glob("#{gem_include}/**/*.h").sort.each do |path|
          rel_path = path.sub("#{gem_include}/", "")
          write_header_content(f, "#{gem.name}: #{rel_path}", path)
          # Also track the relative path for source include transformation
          # (handles includes like #include "io_hal.h")
          @processed_headers << rel_path unless @processed_headers.include?(rel_path)
        end
      end
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
    ].freeze

    def write_gem_sources(f)
      f.puts "\n/* ======== Gem sources ======== */"

      library_gems.each do |gem|
        # Some gems use 'core/' instead of 'src/' (mruby-compiler, mruby-bigint)
        source_dirs = ["#{gem.dir}/src", "#{gem.dir}/core"].select { |d| File.directory?(d) }

        # Include C sources if the gem has any
        unless source_dirs.empty?
          sources = source_dirs.flat_map { |d| Dir.glob("#{d}/**/*.c") }.sort
          sources.each_with_index do |path, idx|
            rel_path = path.sub("#{gem.dir}/", "")
            write_source_content(f, "#{gem.name}: #{rel_path}", path)
            # Clear macros between source files to avoid conflicts
            # (e.g., mrb_stat macro in file.c vs function in file_test.c)
            write_macro_cleanup(f, "#{gem.name}/#{File.basename(path)}") if idx < sources.size - 1
          end
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
        if XMACRO_HEADERS.include?(header)
          xmacro_path = "#{include_dir}/#{header}"
          if File.exist?(xmacro_path)
            xmacro_content = File.read(xmacro_path, mode: "rb")
            "#{prefix}/* Inlined X-macro: #{header} */\n#{xmacro_content}"
          else
            match
          end
        # Comment out all mruby-related includes and any header already in amalgam
        elsif mruby_header?(header) || header == "mruby.h" ||
           @processed_headers.include?(header) || @gem_header_names.include?(header)
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

    def extract_include_guard(content)
      # Match #ifndef GUARD_NAME at start of file (after comments)
      if content =~ /\A(?:\/\*.*?\*\/\s*|\/\/[^\n]*\n)*\s*#ifndef\s+(\w+)\s*\n\s*#define\s+\1/m
        $1
      end
    end

    def strip_include_guard(content, guard)
      return content unless guard

      # Remove opening #ifndef GUARD but KEEP #define GUARD (needed for #ifdef checks)
      content = content.sub(/\A((?:\/\*.*?\*\/\s*|\/\/[^\n]*\n)*\s*)#ifndef\s+#{guard}\s*\n/m, '\1')

      # Remove closing #endif (any comment is ok, guard name may not match exactly)
      content = content.sub(/\n#endif\s*(?:\/\*[^*]*\*\/|\/\/[^\n]*)?\s*\z/m, "\n")

      content
    end

    def transform_includes(content, inline: false)
      # Match both "#include" and "# include" (preprocessor allows spaces)
      # Only match includes at the start of a line (real preprocessor directives)
      content.gsub(/^(\s*)(#\s*include\s+[<"]([^>"]+)[>"])/m) do |match|
        prefix = $1
        include_stmt = $2
        header = $3

        if already_included?(header)
          # Keep original whitespace, comment out the include
          "#{prefix}// #{include_stmt} - in amalgam"
        elsif inline && mruby_header?(header)
          # Recursively inline this header
          "#{prefix}#{inline_header(header)}"
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
  end
end
