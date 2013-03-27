MRuby::Build.new do |conf|
  toolchain :gcc

  conf.gem "#{root}/mrbgems/mruby-sprintf"
  conf.gem "#{root}/mrbgems/mruby-print"

  Dir.glob("#{root}/mrbgems/mruby-*") do |x|
    conf.gem x unless x =~ /\/mruby-(print|sprintf)$/
  end
end
