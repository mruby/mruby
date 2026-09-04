# mrbc only build in a dedicated build directory (build/mrbc) so it does not
# clobber a normal host build.
MRuby::Build.new('mrbc') do |conf|
  if ENV['VisualStudioVersion'] || ENV['VSINSTALLDIR']
    conf.toolchain :visualcpp
  else
    conf.toolchain :gcc
  end

  conf.build_mrbc_exec
  conf.disable_libmruby
end
