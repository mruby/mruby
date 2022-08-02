# Requires Android NDK r13 or later.
MRuby::CrossBuild.new('android-armeabi') do |conf|
  params = {
    :arch => 'armeabi',
    :sdk_version => 26,
    :toolchain => :clang,
  }
  toolchain :android, params

  conf.gembox 'default'
end
