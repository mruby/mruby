require 'pathname'
require 'forwardable'

module MRuby
  module Gem
    class << self
      attr_accessor :current
    end
    LinkerConfig = Struct.new(:libraries, :library_paths, :flags, :flags_before_libraries, :flags_after_libraries) 

    class Specification
      include Rake::DSL
      extend Forwardable
      def_delegators :@build, :filename, :objfile, :libfile, :exefile

      attr_accessor :name, :dir, :build
      alias mruby build
      attr_accessor :build_config_initializer

      attr_accessor :version
      attr_accessor :description, :summary
      attr_accessor :homepage
      attr_accessor :licenses, :authors
      alias :license= :licenses=
      alias :author= :authors=

      attr_accessor :rbfiles, :objs
      attr_accessor :test_objs, :test_rbfiles, :test_args
      attr_accessor :test_preload

      attr_accessor :bins

      attr_accessor :requirements
      attr_reader :dependencies

      attr_block MRuby::Build::COMMANDS

      def initialize(name, &block)
        @name = name
        @initializer = block
        MRuby::Gem.current = self
      end

      def setup
        MRuby::Gem.current = self
        @build.compilers.each do |compiler|
          compiler.include_paths << "#{dir}/include"
        end
        MRuby::Build::COMMANDS.each do |command|
          instance_variable_set("@#{command}", @build.send(command).clone)
        end
        @linker = LinkerConfig.new([], [], [], [])

        @rbfiles = Dir.glob("#{dir}/mrblib/*.rb")
        @objs = Dir.glob("#{dir}/src/*.{c,cpp,m,asm,S}").map do |f|
          objfile(f.relative_path_from(@dir).to_s.pathmap("#{build_dir}/%X"))
        end
        @objs << objfile("#{build_dir}/gem_init")

        @test_rbfiles = Dir.glob("#{dir}/test/*.rb")
        @test_objs = Dir.glob("#{dir}/test/*.{c,cpp,m,asm,S}").map do |f|
          objfile(f.relative_path_from(dir).to_s.pathmap("#{build_dir}/%X"))
        end
        @test_preload = 'test/assert.rb'
        @test_args = {}

        @bins = []

        @requirements = []
        @dependencies = []

        instance_eval(&@initializer)

        if !name || !licenses || !authors
          fail "#{name || dir} required to set name, license(s) and author(s)"
        end

        build.libmruby << @objs

        instance_eval(&@build_config_initializer) if @build_config_initializer

        compilers.each do |compiler|
          compiler.define_rules build_dir, "#{dir}/"
        end

        define_gem_init_builder
      end

      def add_dependency(name, *requirements)
        requirements = ['> 0.0.0'] if requirements.empty?
        requirements.flatten!
        @dependencies << [:gem => name, :requirements => requirements]
      end

      def self.bin=(bin)
        @bins = [bin].flatten
      end

      def build_dir
        "#{build.build_dir}/mrbgems/#{name}"
      end

      def test_rbireps
        "#{build_dir}/gem_test.c"
      end

      def funcname
        @funcname ||= @name.gsub('-', '_')
      end

      def compilers
        MRuby::Build::COMPILERS.map do |c|
          instance_variable_get("@#{c}")
        end
      end

      def define_gem_init_builder
        file objfile("#{build_dir}/gem_init") => "#{build_dir}/gem_init.c"
        file "#{build_dir}/gem_init.c" => [build.mrbcfile] + [rbfiles].flatten do |t|
          FileUtils.mkdir_p build_dir
          generate_gem_init("#{build_dir}/gem_init.c")
        end
      end

      def generate_gem_init(fname)
        open(fname, 'w') do |f|
          print_gem_init_header f
          build.mrbc.run f, rbfiles, "gem_mrblib_irep_#{funcname}" unless rbfiles.empty?
          f.puts %Q[void mrb_#{funcname}_gem_init(mrb_state *mrb);]
          f.puts %Q[void mrb_#{funcname}_gem_final(mrb_state *mrb);]
          f.puts %Q[]
          f.puts %Q[void GENERATED_TMP_mrb_#{funcname}_gem_init(mrb_state *mrb) {]
          f.puts %Q[  int ai = mrb_gc_arena_save(mrb);]
          f.puts %Q[  mrb_#{funcname}_gem_init(mrb);] if objs != [objfile("#{build_dir}/gem_init")]
          unless rbfiles.empty?
            f.puts %Q[  mrb_load_irep(mrb, gem_mrblib_irep_#{funcname});]
            f.puts %Q[  if (mrb->exc) {]
            f.puts %Q[    mrb_p(mrb, mrb_obj_value(mrb->exc));]
            f.puts %Q[    exit(EXIT_FAILURE);]
            f.puts %Q[  }]
          end
          f.puts %Q[  mrb_gc_arena_restore(mrb, ai);]
          f.puts %Q[}]
          f.puts %Q[]
          f.puts %Q[void GENERATED_TMP_mrb_#{funcname}_gem_final(mrb_state *mrb) {]
          f.puts %Q[  mrb_#{funcname}_gem_final(mrb);] if objs != [objfile("#{build_dir}/gem_init")]
          f.puts %Q[}]
        end
      end # generate_gem_init

      def print_gem_init_header(f)
        f.puts %Q[/*]
        f.puts %Q[ * This file is loading the irep]
        f.puts %Q[ * Ruby GEM code.]
        f.puts %Q[ *]
        f.puts %Q[ * IMPORTANT:]
        f.puts %Q[ *   This file was generated!]
        f.puts %Q[ *   All manual changes will get lost.]
        f.puts %Q[ */]
        f.puts %Q[#include <stdlib.h>]
        f.puts %Q[#include "mruby.h"]
        f.puts %Q[#include "mruby/irep.h"]
        f.puts %Q[#include "mruby/dump.h"]
        f.puts %Q[#include "mruby/string.h"]
        f.puts %Q[#include "mruby/proc.h"]
        f.puts %Q[#include "mruby/variable.h"]
        f.puts %Q[#include "mruby/array.h"]
        f.puts %Q[#include "mruby/hash.h"]
      end

    end # Specification

    class List < Array
      def <<(gem)
        fail ArgumentError.new("Don't find directory for this GEM") unless gem.respond_to? :dir
        unless include?(gem)
          super(gem)
        else
          # GEM was already added to this list
        end
      end

      # we assume that a gem with the same directory is equal
      def include?(gem)
        detect {|g| g.dir == gem.dir }
      end
    end # List
  end # Gem

  GemBox = Object.new
  class << GemBox
    def new(&block); block.call(self); end
    def config=(obj); @config = obj; end
    def gem(gemdir, &block); @config.gem(gemdir, &block); end
  end # GemBox
end # MRuby
