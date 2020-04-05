module MRuby
	class ExecTools
		MRBC_FILE = 'mrbc'
		BIN_PATH = ENV['INSTALL_DIR'] || "#{MRUBY_ROOT}/bin"
		BIN_TASK = []

		def self.hire(build_dir)
			new(build_dir, BIN_PATH)
		end

		def self.add_taskbin(build_exe, bin_exe)
			return if MRuby::Build.current.kind_of?(MRuby::CrossBuild)
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

		attr_reader :build_dir, :bin_path
		attr_writer :extension

		def initialize(build_dir, bin_path)
			@active = true
			@list = []
			@build_dir = Pathname.new(build_dir)
			@bin_path = Pathname.new(bin_path)
			@build_real = @build_dir.children(false).include?('bin')
		end

		def <<(basename)
			return unless @active
			@list << basename
			ext = basename.match(/\.([\d\w]+)\z/)
			ext = ext ? ext[1] : @extension
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

		def mrbc
			exe(MRBC_FILE)
		end

		def exe(name)
			if MRuby::Build.current.name == 'host'
				build_path(name)
			elsif exist?(name)
				bin_path(name)
			elsif host_target?
				MRuby.targets['host'].mrbcfile
			else
				return nil
			end
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