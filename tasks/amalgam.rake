require_relative '../lib/mruby/amalgam'

MRuby.each_target do
  next unless libmruby_enabled?

  amalgam_dir = "#{build_dir}/amalgam"
  header_file = "#{amalgam_dir}/mruby.h"
  source_file = "#{amalgam_dir}/mruby.c"

  # Header depends on presym generation (for presym headers)
  file header_file => [:gensym] do |t|
    amalgam = MRuby::Amalgam.new(self)
    amalgam.generate_header(t.name)
  end

  # Source depends on generated files (mrblib.c, gem_init.c, etc.)
  mrblib_src = "#{build_dir}/mrblib/mrblib.c"
  gem_init_src = "#{build_dir}/mrbgems/gem_init.c"

  source_deps = [:gensym]
  source_deps << mrblib_src if File.exist?(mrblib_src) || libmruby_enabled?

  file source_file => source_deps do |t|
    amalgam = MRuby::Amalgam.new(self)
    amalgam.generate_source(t.name)
  end

  # mruby-compiler (and its vendored prism) cannot share a translation
  # unit with the core sources; it gets its own amalgamated file
  amalgam_files = [header_file, source_file]
  if gems.any? { |g| MRuby::Amalgam::SEPARATE_TU_GEMS.include?(g.name) }
    compiler_file = "#{amalgam_dir}/mruby_compiler.c"
    file compiler_file => source_deps do |t|
      amalgam = MRuby::Amalgam.new(self)
      amalgam.generate_compiler_source(t.name)
    end
    amalgam_files << compiler_file
  end

  desc "Generate amalgamated mruby.h and mruby.c in #{amalgam_dir}"
  task :amalgam => amalgam_files do
    puts "Amalgamation complete:"
    puts "  Header: #{header_file}"
    puts "  Source: #{source_file}"
    puts "  Compiler source: #{amalgam_files[2]}" if amalgam_files.size > 2
  end
end
