require 'tsort'

module MRuby
  class RbfilesSorter
    include TSort

    TAG_RXP = /^#\s*require:\s*['"]([\w\.\/]+)["']\s*$/

    def initialize(root)
      @root = root
      @files = Dir.glob("#{root}/**/*.rb").sort
      @deps = {}
    end

    def sort
      @files.each { |f| parse_deps(f) }
      tsort
    end

    def tsort_each_child(node, &block)
      @deps[node].each(&block)
    end

    def tsort_each_node(&block)
      @deps.each_key(&block)
    end

    def parse_deps(file)
      f = File.new(file)

      deps_list = @deps[File.expand_path(file)] = []

      f.each_line do |line|
        # Skip blank lines
        next if line =~ /^\s*$/
        # All requires should be in the beginning of the file
        break if line !~ TAG_RXP

        dep = line.match(TAG_RXP)[1]

        dep += ".rb" unless dep.end_with?(".rb")

        deps_list << File.expand_path(dep, File.dirname(file))
      end
    end
  end
end
