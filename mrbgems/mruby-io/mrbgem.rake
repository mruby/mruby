MRuby::Gem::Specification.new('mruby-io') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Internet Initiative Japan Inc.', 'mruby developers']
  spec.summary = 'IO and File class'

  spec.cc.include_paths << "#{build.root}/src"

  case RUBY_PLATFORM
  when /mingw|mswin|msys/
    really_for_window = true

    if build.kind_of?(MRuby::CrossBuild)
      if %w(x86_64-w64-mingw32 i686-w64-mingw32).include?(build.host_target)
        really_for_window = true
      else
        really_for_window = false
      end
    end

    if really_for_window
      spec.linker.libraries += ['Ws2_32']
      #spec.cc.include_paths += ["C:/Windows/system/include"]
      spec.linker.library_paths += ["C:/Windows/system"]
    end
  end
  if build.kind_of?(MRuby::CrossBuild) && %w(x86_64-w64-mingw32 i686-w64-mingw32).include?(build.host_target)
    spec.linker.libraries += ['ws2_32']
  end
  spec.add_test_dependency 'mruby-time', core: 'mruby-time'
end
