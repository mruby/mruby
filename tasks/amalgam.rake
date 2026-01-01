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

  desc "Generate amalgamated mruby.h and mruby.c in #{amalgam_dir}"
  task :amalgam => [header_file, source_file] do
    puts "Amalgamation complete:"
    puts "  Header: #{header_file}"
    puts "  Source: #{source_file}"
  end
end
