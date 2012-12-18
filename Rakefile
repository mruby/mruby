# Build description.
# basic build file for mruby

# compiler, linker (gcc), archiver, parser generator
CC = ENV['CC'] || 'gcc'
LL = ENV['LL'] || 'gcc'
AR = ENV['AR'] || 'ar'
YACC = ENV['YACC'] || 'bison'
MAKE = ENV['MAKE'] || 'make'

# mruby source root path
MRUBY_ROOT = ENV['MRUBY_ROOT'] || File.expand_path(File.dirname(__FILE__))

# by default GEMs are deactivated
ENABLE_GEMS = true

# the default file which contains the active GEMs
ACTIVE_GEMS = File.join(File.dirname(__FILE__), 'mrbgems', 'GEMS.active')

# default compile option
COMPILE_MODE = :debug


##############################
# compile flags

case COMPILE_MODE.to_s
when 'release'
  CFLAGS = ['-O3']
when 'small'
  CFLAGS = ['-Os']
else  # including 'debug'
  e = ENV['CFLAGS']
  CFLAGS = if e then [e] else ['-g', '-O3'] end
end
LDFLAGS = [ENV['LDFLAGS']]
LIBS = [ENV['LIBS']]

CFLAGS << "-Wall" << "-Werror-implicit-function-declaration" << "-I#{MRUBY_ROOT}/include"
if ENV['OS'] == 'Windows_NT'
  MAKE_FLAGS = "--no-print-directory CC=#{CC} LL=#{LL} CFLAGS='#{CFLAGS.join(' ')}' LDFLAGS='#{LDFLAGS.join(' ')}' LIBS='#{LIBS.join(' ')}' ENABLE_GEMS='#{ENABLE_GEMS}' MRUBY_ROOT='#{MRUBY_ROOT}'"
else
  MAKE_FLAGS = "--no-print-directory CC='#{CC}' LL='#{LL}' CFLAGS='#{CFLAGS.join(' ')}' LDFLAGS='#{LDFLAGS.join(' ')}' LIBS='#{LIBS.join(' ')}' ENABLE_GEMS='#{ENABLE_GEMS}' MRUBY_ROOT='#{MRUBY_ROOT}'"
end



##############################
# internal variables

CP = ENV['CP'] ||= 'cp'
RM_F = ENV['RM_F'] ||= 'rm -f'
CAT = ENV['CAT'] ||= 'cat'


##############################
# generic build targets, rules
if ENABLE_GEMS
  require './mrbgems/build_tasks'
end

task :default => :all

desc "build all targets, install (locally) in-repo"
task :all do
  sh "#{MAKE} -C src #{MAKE_FLAGS}"
  sh "#{MAKE} -C mrblib #{MAKE_FLAGS}"
  if ENABLE_GEMS
    puts "-- MAKE mrbgems --"
    Rake::Task['mrbgems_all'].invoke
  end
  sh "#{MAKE} -C tools/mruby #{MAKE_FLAGS}"
  sh "#{MAKE} -C tools/mirb #{MAKE_FLAGS}"
end

desc "sh all mruby tests"
task :test => [:all] do
  sh "#{MAKE} -C test #{MAKE_FLAGS}"
end

desc "clean all built and in-repo installed artifacts"
task :clean do
  sh "#{MAKE} clean -C src #{MAKE_FLAGS}"
  sh "#{MAKE} clean -C mrblib #{MAKE_FLAGS}"
  if ENABLE_GEMS
    puts "-- MAKE mrbgems --"
    Rake::Task['mrbgems_clean'].invoke
  end
  sh "#{MAKE} clean -C tools/mruby #{MAKE_FLAGS}"
  sh "#{MAKE} clean -C tools/mirb #{MAKE_FLAGS}"
  sh "#{MAKE} clean -C test #{MAKE_FLAGS}"
end

desc "show build config summary"
task :showconfig do
  puts "  CC = #{CC}"
  puts "  LL = #{LL}"
  puts "  MAKE = #{MAKE}"
  puts ""
  puts "  CFLAGS = #{CFLAGS.join(' ')}"
  puts "  LDFLAGS = #{LDFLAGS.join(' ')}"
  puts "  LIBS = #{LIBS.join(' ')}"
end
