module MRuby
	class ExecTools
		MRBC_FILE = 'mrbc'
		BUILD_PATH = ENV['MRUBY_BUILD_DIR'] || "#{MRUBY_ROOT}/build"
		BIN_PATH = ENV['INSTALL_DIR'] || "#{MRUBY_ROOT}/bin"
		BIN_TASK = []

		def self.hire(build_dir)
			new(build_dir, BIN_PATH)
		end

		def self.add_taskbin(build_exe, bin_exe)
			return if MRuby::Build.current.kind_of?(MRuby::CrossBuild)
			return if [build_exe, bin_exe].any?(&:nil?)
			unless BIN_TASK.map(&:values).include?(build_exe)
				BIN_TASK << { src: build_exe, output: bin_exe }
			end
		end

		def self.generate_task(&block)
			BIN_TASK.map do |task_properti|
				Rake::FileTask.define_task(task_properti[:src] => task_properti[:output], &block)
				task_properti[:src]
			end
		end

		def self.list_build
			Pathname.new(BUILD_PATH).children(false).map(&:to_s)
		end

		attr_reader :build_dir, :bin_path
		attr_writer :extension

		def initialize(build_dir, bin_path)
			@active = true
			@list = []
			@build_dir = Pathname.new(build_dir)
			@bin_path = Pathname.new(bin_path)

			build_bin_dir = @build_dir.join('bin')
			@build_dir.mkpath if Dir[@build_dir].empty?
			build_bin_dir.mkpath if Dir[build_bin_dir].empty?
		end

		def <<(basename)
			return unless @active
			@list << basename
			ext = File.extname(basename)
			ext = @extension if ext.empty?
			self.class.add_taskbin(bin_path(basename, ext), build_path(basename, ext))
		end

		def disable
			@active = false
		end

		def disable?
			!@active
		end

		def empty?
			disable? || @list.empty?
		end

		def all_executable
			@list.join(", ")
		end

		def mrbc
			executable = exe(MRBC_FILE)
			return executable if File.exist?(executable)
			return MRuby.targets['host'].mrbcfile if host_target?
			raise RuntimeError, "Couldn't find mrbc.exe \nBuild host first."
		end

		def exe(name)
			exist?(name) ? bin_path(name) : build_path(name)
		end

		private
		def exist?(name)
			File.exist?(bin_path(name))
		end

		def build_path(name, ext = @extension)
			@build_dir.join('bin').join(name).to_s + ext
		end

		def bin_path(name, ext = @extension)
			@bin_path.join(name).to_s + ext
		end

		def host_target?
			!MRuby.targets['host'].nil?
		end
	end
end
