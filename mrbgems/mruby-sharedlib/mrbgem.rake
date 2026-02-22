module MRuby
  class Build
    def mruby_sharedlib_ext
      (`uname` =~ /darwin/i)? 'dylib' : (ENV['OS'] == 'Windows_NT')? 'dll' : 'so'
    end

    def exefile(name)
      if name.is_a?(Array)
        name.flatten.map { |n| exefile(n) }
	  elsif name !~ /\./
        "#{name}#{exts.executable}"
      else
        name
      end
    end
  end
end

MRuby.each_target do
  next if kind_of? MRuby::CrossBuild

  if ENV['OS'] == 'Windows_NT'
    mruby_sharedlib = "#{build_dir}/bin/mruby.#{mruby_sharedlib_ext}"
    @bins << "mruby.#{mruby_sharedlib_ext}"
  else
    mruby_sharedlib = "#{build_dir}/bin/libmruby.#{mruby_sharedlib_ext}"
    @bins << "libmruby.#{mruby_sharedlib_ext}"
  end

  is_vc = cc.command =~ /^cl(\.exe)?$/
  is_mingw = ENV['OS'] == 'Windows_NT' && cc.command =~ /^gcc/
  unless is_vc or is_mingw
    cc.flags << '-fPIC'
  end
  file mruby_sharedlib => libfile("#{build_dir}/lib/libmruby") do |t|
    deffile = "#{File.dirname(__FILE__)}/mruby.def"

    gem_flags = gems.map { |g| g.linker.flags }
    gem_flags << (is_vc ? '/DLL' : '-shared')
    gem_flags << (is_vc ? "/DEF:#{deffile}" : mruby_sharedlib_ext == 'dylib'? '-Wl,-force_load' : is_mingw ? deffile : "-Wl,--whole-archive")
    gem_flags += t.prerequisites
    gem_libraries = gems.map { |g| g.linker.libraries }
    gem_library_paths = gems.map { |g| g.linker.library_paths }
    gem_flags_before_libraries = gems.map { |g| g.linker.flags_before_libraries }
    gem_flags_after_libraries = gems.map { |g| g.linker.flags_after_libraries }
    unless is_vc or is_mingw
      gem_flags_after_libraries = "-Wl,--no-whole-archive"
    end
    linker.run t.name, [], gem_libraries, gem_library_paths, gem_flags, gem_flags_before_libraries, gem_flags_after_libraries
  end
end
