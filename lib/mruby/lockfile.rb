autoload :YAML, 'yaml'

module MRuby
  autoload :Source, 'mruby/source'

  class Lockfile
    class << self
      def enable
        @enabled = true
      end

      def disable
        @enabled = false
      end

      def enabled?
        @enabled
      end

      # def build(target_name)
      #   instance.build(target_name)
      # end

      def write
        instance.write if enabled?
      end

      def print_new_version
        @instance.print_new_version if enabled?
      end

      def instance
        @instance ||= new("#{MRUBY_CONFIG}.lock")
      end
    end

    def initialize(filename)
      @filename = filename
    end

    def refresh_builds
      @builds = nil
      read.empty? ? false : @builds
    end

    # def build(target_name)
    #   read[target_name] ||= {}
    # end

    def write
      locks = Hash.new
      locks["mruby_version"] = mruby
      locks["builds"] = @builds if refresh_builds
      File.write(@filename, YAML.dump(locks))
    end

    def print_new_version
      flatten_builds = Proc.new do |b, func|
        next [nil] unless b
        [b['mruby_version']['release_no'], *func[b["builds"], func]].compact
      end

      history = flatten_builds[refresh_builds, flatten_builds].compact
      list = list_build(MRuby::Build::BUILD_DIR)
      return if history.empty? || list.empty?

      available = Hash.new
      list.each do |name_build|
        past = history.find { |build| build.keys.include?(name_build) }
        next unless past
        mv = past[name_build].to_s.match(/(\d{1,2})(\d{2})(\d{2})$/)
        dest_version = "%s(%d.%d.%d)" % [name_build, mv[1].to_i, mv[2].to_i, mv[3].to_i]
        src_version = MRuby::Source::MRUBY_VERSION

        if past[name_build] < MRuby::Source::MRUBY_RELEASE_NO
          available[dest_version] = src_version
        end
      end

      return if available.empty?

      puts "================================================"
      puts " Upgrade Version Avaible"
      available.each { |version| puts "%20s to %s" % version }
      puts "================================================"
    end

    private

    def read
      @builds ||= File.exist?(@filename) ? YAML.load_file(@filename) || {} : {}
    end

    def shellquote(s)
      if ENV['OS'] == 'Windows_NT'
        "\"#{s}\""
      else
        "'#{s}'"
      end
    end

    def mruby
      mruby = Hash.new
      mruby['version'] = MRuby::Source::MRUBY_VERSION
      mruby['release_no'] = Hash.new
      MRuby.each_target do |build|
        mruby['release_no'][build.name] = MRuby::Source::MRUBY_RELEASE_NO
      end

      git_dir = "#{MRUBY_ROOT}/.git"
      if File.directory?(git_dir)
        mruby['git_commit'] = `git --git-dir #{shellquote(git_dir)} --work-tree #{shellquote(MRUBY_ROOT)} rev-parse --verify HEAD`.strip
      end

      mruby
    end

    def list_build(path)
      Pathname.new(path).children(false).map(&:to_s)
    end

    enable
  end
end
