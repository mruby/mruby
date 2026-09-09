MRuby::Toolchain.new(:zig) do |conf, params|
  toolchain :clang

  zig = ENV['ZIG'] || 'zig'
  target = params[:target]

  conf.cc.command = "#{zig} cc"
  conf.cxx.command = "#{zig} c++"
  conf.linker.command = "#{zig} cc"
  conf.archiver.command = "#{zig} ar"

  if target
    flags = %W(-target #{target})
    [conf.cc, conf.cxx, conf.linker].each{|tool| tool.flags << flags}
  end

  # `zig ar` is llvm-ar, which defaults to the Darwin format on macOS hosts,
  # where it overflows on long member paths.
  conf.archiver.archive_options = '--format=gnu rcs%{deterministic} "%{outfile}" %{objs}'
end
