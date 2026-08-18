MRuby::Toolchain.new(:wasi) do |conf, params|
  toolchain :clang

  wasi_sdk = ENV['WASI_SDK_PATH'] || '/opt/wasi-sdk'
  bin = "#{wasi_sdk}/bin"
  target = params[:target] || 'wasm32-wasip1'

  # setjmp/longjmp via native WASM exception handling; requires wasi-sdk 26 or later
  flags = %W(--target=#{target} --sysroot=#{wasi_sdk}/share/wasi-sysroot
             -mllvm -wasm-enable-sjlj -mllvm -wasm-use-legacy-eh=false)

  conf.cc.command = "#{bin}/clang"
  conf.cxx.command = "#{bin}/clang++"
  conf.linker.command = "#{bin}/clang"
  conf.archiver.command = "#{bin}/llvm-ar"

  [conf.cc, conf.cxx, conf.linker].each{|tool| tool.flags << flags}
  conf.linker.libraries << 'setjmp'

  # llvm-ar defaults to the Darwin format on macOS hosts, which overflows on long member paths
  conf.archiver.archive_options = '--format=gnu rcs "%{outfile}" %{objs}'
end
