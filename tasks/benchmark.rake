module MRuby
  module Benchmark
    REPEAT = 8
    BENCHMARKS_PER_PLOT = 12

    def data_files
      @data_files ||= []
    end

    def title(target_name)
      case build_config_name
      when 'cc'
        target_name.sub(/march\-(\w+)/, 'march=\1')
                   .gsub('-cxx-abi', ' (C++ ABI)')
                   .gsub('-', ' -')
      else
        target_name
      end
    end

    def dir
      File.join MRUBY_ROOT, 'benchmark'
    end

    def bm_files
      Dir.glob(File.join dir, 'bm_*.rb')
    end

    def build_config_name
      if ENV['MRUBY_CONFIG']
        File.basename(ENV['MRUBY_CONFIG'], '.rb').gsub('build_config_', '')
      else
        "build"
      end
    end

    def plot_file(index = nil)
      File.join(dir, "#{build_config_name}#{index}.pdf")
    end

    def plot_opts_file
      File.join dir, 'plot.gpl'
    end

    def data_files_per_target
      @data_files_per_target ||= data_files.group_by {|f| File.dirname(f).split(File::SEPARATOR)[-1]}
    end

    def plot_files
      h = data_files_per_target
      (0...(h[h.keys.first].size / BENCHMARKS_PER_PLOT.to_f).ceil).map do |i|
        plot_file i
      end
    end

    def all_data
      return @all_data if @all_data

      all_data = {}
      files = data_files_per_target
      files.each do |target_name, data_files|
        bm_data = data_files.map do |dat_file|
          bm_name = File.basename dat_file, '.*'
          cpu_times = File.read(dat_file).each_line.map do |l|
            real, sys, user, res = l.split(/\s+/).map(&:to_f)

            sys + user
          end
          
          cpu_times.sort!
          min = cpu_times.shift.round 3
          max = cpu_times.pop.round 3
          avg = cpu_times.inject(&:+)./(cpu_times.size).round(3)

          [bm_name, avg, min, max]
        end

        # underscore means subscript in gnuplot
        all_data[target_name.gsub('_', '-')] = bm_data
      end

      @all_data = all_data
    end

    def sliced_data
      return @sliced_data if @sliced_data

      sliced_data = []
      all_data.each do |k, v|
        v.each_slice(MRuby::Benchmark::BENCHMARKS_PER_PLOT).with_index do |slice, index|
          sliced_data[index] ||= {}
          hash = sliced_data[index]
          hash[k] = slice
        end
      end

      @sliced_data = sliced_data
    end

    def plot(index = nil)
      data = if index
        sliced_data[index]
      else
        all_data
      end

      opts_file = plot_opts_file
      opts = File.read(opts_file).each_line.to_a.map(&:strip).join(';')

      opts += ";set output '#{plot_file index}'"
      opts += ";set key off" if index && index > 0
      opts += ';plot '

      opts += data.keys.map.with_index do |k, i|
        %Q['-' u 2:3:4:xtic(1) w hist title columnheader(1)]
      end.join(',')
      opts += ';'

      cmd = %Q{gnuplot -p -e "#{opts}"}

      IO.popen(cmd, 'w') do |p|
        data.each do |k, v|
          p.puts %Q["#{title  k}"]
          v.each do |l|
            p.puts l.join(' ')
          end
          p.puts "e"
        end
      end
    end

    def print(relative = nil)
      data = Hash.new{|h, k| h[k] = []}
      self.all_data.each do |bc_name, bms|
        bms.each do |(bm_name, *bm_data)|
          data[bm_name] << bm_data
        end
      end
  
      row_headers = all_data.keys.map(&:to_s)
      max_row_wid = row_headers.max_by(&:size).size + 2

      col_headers = data.keys.map(&:to_s)
      max_col_wid = col_headers.max_by(&:size).size + 2

      puts " ".*(max_col_wid) + row_headers.map {|h| "%#{max_row_wid}s" % h }.join('')
      puts
      data.each do |bm_name, bm_data|
        avgs = bm_data.map do |(avg, *_)| 
          if relative
            avg / bm_data[relative][0]
          else
            avg
          end
        end
        printf "%#{max_col_wid}s" % bm_name
        puts avgs.map {|avg| "%#{max_row_wid}.2f" % avg }.join('')
      end
    end

    include Rake::DSL

    def define_tasks
      plot_files.each_with_index do |plot_file, index|
        file plot_file => [*data_files, plot_opts_file] do
          plot index
        end
      end

      namespace :benchmark do
        task :plot => plot_files do
        end

        task :print => data_files do |t, args|
          print args[0] ? args[0].to_i : nil
        end
      end
    end

    def define_bm_task(mruby_bin, bm_file, target_name)
      bm_name = File.basename bm_file, ".rb"
      data_dir = File.join(dir, build_config_name, target_name)
      data_file = File.join(data_dir, "#{bm_name}.dat")
      self.data_files << data_file

      directory data_dir

      file data_file => [bm_file, data_dir, mruby_bin] do |task|
        puts bm_name

        File.open(task.name, "w") do |f|
          REPEAT.times do
            # %e real
            # %S sys
            # %U user
            # %M max. resident size
            f.puts %x{(time -f "%e %S %U %M" #{mruby_bin} #{bm_file}) 2>&1 >/dev/null}
          end
        end
      end
    end


    extend self
  end
end

module MRuby
  module Profiling
    def data_files
      @data_files ||= Hash.new{|h, k| h[k] = []}
    end

    def dir
      File.join MRUBY_ROOT, 'profiling'
    end

    def plot_opts_file
      File.join dir, 'plot.gpl'
    end

    def plot_files
      @plot_files ||= []
    end

    def plot_file(target_name)
      File.join(dir, "#{MRuby::Benchmark.build_config_name}_#{target_name}.pdf")
    end

    def plot(plot_file, target_name)
      opts_file = plot_opts_file
      opts = File.read(opts_file).each_line.to_a.map(&:strip).join(';')

      opts += ";set output '#{plot_file}'"

      data_files = self.data_files[target_name]

      syms = []
      totals = {}
      bm_names = []
      data = Hash.new{|h, k| h[k] = Hash.new{|h, k| h[k] = 0.0}}
      data_files.each_with_index do |data_file, index|
        bm_name = "#{File.basename(data_file, ".dat")}"
        bm_names[index] = bm_name
        totals[bm_name] = 0
        File.read(data_file).each_line do |line|
          perc, sym = line.split /\s+/
          data[bm_name][sym] = perc.to_f;
          totals[bm_name] += perc.to_f;
          syms << sym
        end
      end

      syms.uniq!

      opts += ';plot '
      opts += syms.map.with_index do |bm_name, index|
        %Q['-' u 2:xtic(1) t columnheader(1) ]
      end.join(',')
      opts += ';'

      cmd = %Q{gnuplot -p -e "#{opts}"}

      IO.popen(cmd, 'w') do |p|
        syms.each_with_index do |sym, index|
          p.puts sym
          bm_names.each do |bm_name|
            p.puts "#{bm_name} #{data[bm_name][sym].to_f / totals[bm_name] * 100 }"
          end
          p.puts 'e'
        end
      end
    end

    include Rake::DSL

    def define_target_task(target_name)
      plot_file = plot_file(target_name)
      self.plot_files << plot_file
      file plot_file => [*data_files[target_name], plot_opts_file] do
        plot plot_file, target_name
      end
    end

    def define_tasks
      namespace :profiling do
        task :print => data_files do |t, args|
        end

        task :plot => plot_files do
        end
      end
    end

    def define_bm_task(mruby_bin, bm_file, target_name)
      bm_name = File.basename bm_file, ".rb"
      data_dir = File.join('profiling', MRuby::Benchmark.build_config_name, target_name)
      data_file = File.join(data_dir, "#{bm_name}.dat")
      session_dir = '/tmp/mruby_oprofile'
      self.data_files[target_name] << data_file

      directory data_dir
      directory session_dir

      file data_file => [bm_file, data_dir, mruby_bin, session_dir] do |task|
        print bm_name
        puts "..."

        sh "operf -d /tmp/mruby_oprofile #{mruby_bin} #{bm_file}"
        out = %x{opreport --session-dir=#{session_dir} --no-header --threshold 3 --symbols}

        File.open(task.name, "w") do |f|
          out.each_line do |line|
            if line =~ /^\d+\s+(\d+(?:\.\d+)?)\s+(?:.*?\s+)?([a-zA-Z0-9_]+)$/
              f.puts "#{$1} #{$2}"
            end
          end
        end
      end
    end

    extend self
  end
end

MRuby.each_target do |target|
  next if target.name == 'host'
  mruby_bin = "#{target.build_dir}/bin/mruby"

  MRuby::Benchmark.bm_files.each do |bm_file|
    bm_name = File.basename bm_file, ".rb"

    MRuby::Benchmark.define_bm_task mruby_bin, bm_file, target.name
    MRuby::Profiling.define_bm_task mruby_bin, bm_file, target.name
  end

  MRuby::Profiling.define_target_task target.name 
end

MRuby::Benchmark.define_tasks 
MRuby::Profiling.define_tasks

