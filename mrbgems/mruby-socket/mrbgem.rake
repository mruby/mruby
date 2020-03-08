MRuby::Gem::Specification.new('mruby-socket') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Internet Initiative Japan', 'mruby developers']
  spec.summary = 'standard socket class'

  spec.cc.include_paths << "#{build.root}/src"
  #spec.cc.defines << "HAVE_SA_LEN=0"

  # If Windows, use winsock
  if ( /mswin|mingw|win32/ =~ RUBY_PLATFORM ) then
    really_for_window = true

    if build.kind_of?(MRuby::CrossBuild)
      if %w(x86_64-w64-mingw32 i686-w64-mingw32).include?(build.host_target)
        really_for_window = true
      else
        really_for_window = false
      end
    end

    if really_for_window
      spec.linker.libraries << "wsock32"
      spec.linker.libraries << "ws2_32"
    end
  end

  spec.add_dependency('mruby-io', :core => 'mruby-io')
  spec.add_dependency('mruby-pack', :core => 'mruby-pack')
  # spec.add_dependency('mruby-mtest')
end
