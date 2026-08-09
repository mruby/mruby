# Requires wasi-sdk 26 or later; set WASI_SDK_PATH unless it is in /opt/wasi-sdk.
#
# Currently unsupported on wasm32-wasip1: "mruby-io", "mruby-dir", "mruby-socket"
MRuby::CrossBuild.new('wasi') do |conf|
  conf.toolchain :wasi

  conf.gembox 'default-no-stdio'
  conf.gembox 'stdlib-ext'
  conf.gembox 'metaprog'

  conf.gem core: 'mruby-bin-mruby'
  conf.gem core: 'mruby-bin-mrbc'
  conf.gem core: 'mruby-bin-strip'
  conf.gem core: 'mruby-bin-config'
end
