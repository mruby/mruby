#!/usr/bin/env ruby
#
# mrbgems test runner
#

DEPEND_GEMS = []
gemname = File.basename(File.dirname(File.expand_path __FILE__))

if __FILE__ == $0
  repository, dir = 'https://github.com/mruby/mruby.git', 'tmp/mruby'
  build_args = ARGV
  if ARGV.first && ARGV.first.include?('iij')
    repository, dir = 'https://github.com/iij/mruby.git', 'tmp/iij-mruby'
    build_args = ARGV[1, ARGV.size]
  end
  build_args = ['all', 'test']  if build_args.nil? or build_args.empty?

  Dir.mkdir 'tmp'  unless File.exist?('tmp')
  unless File.exist?(dir)
    system "git clone #{repository} #{dir}"
  end

  exit system(%Q[cd #{dir}; MRUBY_CONFIG=#{File.expand_path __FILE__} ruby minirake #{build_args.join(' ')}])
end

MRuby::Build.new do |conf|
  toolchain :gcc
  conf.gems.clear
  conf.gem File.expand_path(File.dirname(__FILE__))
  DEPEND_GEMS.each do |g|
    conf.gem g
  end
end
