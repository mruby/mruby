module MRuby
  module Benchmark
    REPEAT = 8
    BENCHMARKS_PER_PLOT = 12

    def dat_files
      @dat_files ||= []
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

    def bm_files
      Dir.glob("#{MRUBY_ROOT}/benchmark/bm_*.rb")
    end

    def build_config_name
      if ENV['MRUBY_CONFIG']
        File.basename(ENV['MRUBY_CONFIG'], '.rb').gsub('build_config_', '')
      else
        "build"
      end
    end

    def plot_file(index = nil)
      File.join(MRUBY_ROOT, 'benchmark', "#{build_config_name}#{index}.pdf")
    end

    def plot_opts_file
      "#{MRUBY_ROOT}/benchmark/plot.gpl"
    end

    def dat_files_per_target
      @dat_files_per_target ||= dat_files.group_by {|f| File.dirname(f).split(File::SEPARATOR)[-1]}
    end

    def plot_files
      h = dat_files_per_target
      (0...(h[h.keys.first].size / BENCHMARKS_PER_PLOT.to_f).ceil).map do |i|
        plot_file i
      end
    end

    def all_data
      return @all_data if @all_data

      all_data = {}
      files = dat_files_per_target
      files.each do |target_name, dat_files|
        bm_data = dat_files.map do |dat_file|
          bm_name = File.basename dat_file, '.*'
          cpu_times = File.read(dat_file).each_line.map do |l|
            real, sys, user, res = l.split(/\s+/).map(&:to_f)
            
	    sys + user
          end
          
          min = cpu_times.min
          max = cpu_times.max
          avg = cpu_times.inject(&:+) / cpu_times.size

          [bm_name, avg, min, max]
        end.sort_by{|e| e[1]}.reverse

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
      opts += ';plot '

      opts += data.keys.map do
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

    extend self
  end
end



MRuby.each_target do |target|
  next if target.name == 'host'
  mruby_bin = "#{target.build_dir}/bin/mruby"

  MRuby::Benchmark.bm_files.each do |bm_file|
    bm_name = File.basename bm_file, ".rb"

    dat_dir = File.join('benchmark', MRuby::Benchmark.build_config_name, target.name)
    dat_file = File.join(dat_dir, "#{bm_name}.dat")
    MRuby::Benchmark.dat_files << dat_file

    directory dat_dir

    file dat_file => [bm_file, dat_dir, mruby_bin] do |task|
      print bm_name
      puts "..."

      File.open(task.name, "w") do |f|
        MRuby::Benchmark::REPEAT.times do
          # %e real
          # %S sys
          # %U user
          # %M max. resident size
          f.puts %x{(time -f "%e %S %U %M" #{mruby_bin} #{bm_file}) 2>&1 >/dev/null}
        end
      end
    end
  end
end

namespace :benchmark do

  MRuby::Benchmark.plot_files.each_with_index do |plot_file, index|
    file plot_file => [*MRuby::Benchmark.dat_files, MRuby::Benchmark.plot_opts_file] do
      MRuby::Benchmark.plot index
    end
  end

  task :plot => MRuby::Benchmark.plot_files do
  end

  task :print, [:relative] => MRuby::Benchmark.dat_files do |t, args|
    MRuby::Benchmark.print args[:relative] ? args[:relative].to_i : nil
  end
end
