class MRuby::Toolchain::Android

  DEFAULT_ARCH = 'armeabi' # TODO : Revise if arch should have a default

  DEFAULT_TOOLCHAIN = :clang

  DEFAULT_NDK_HOMES = %w{
    /usr/local/opt/android-ndk
    /usr/local/opt/android-sdk/ndk-bundle
    %LOCALAPPDATA%/Android/android-sdk/ndk-bundle
  }

  TOOLCHAINS = [:clang] # TODO : Add gcc support

  ARCHITECTURES = %w{
    armeabi armeabi-v7a arm64-v8a
    x86 x86_64
  } # TODO : Add mips mips64 support

  class AndroidNDKHomeNotFound < StandardError
    def message
        <<-EOM
Couldn't find Android NDK Home.
Set ANDROID_NDK_HOME environment variable or set :ndk_home parameter
        EOM
    end
  end

  class PlatformDirNotFound < StandardError
    def message
        <<-EOM
Couldn't find Android NDK platform directories.
Set ANDROID_PLATFORM environment variable or set :platform parameter
        EOM
    end
  end

  attr_reader :params

  def initialize(params)
    @params = params
  end

  def bin_gcc(command)
    case toolchain
    when :gcc then bin(command)
    when :clang
      command = command.to_s

      command = case arch
        when /armeabi/, /armeabi-v7a/ then 'arm-linux-androideabi-'
        when /arm64-v8a/              then 'aarch64-linux-android-'
        when /x86/                    then 'i686-linux-android-'
        when /x86_64/                 then 'x86_64-linux-android-'
        when /mips/                   then 'mipsel-linux-android-'
        when /mips64/                 then 'mips64el-linux-android-'
        end + command

      prefix = case arch
        when /armeabi/, /armeabi-v7a/ then 'arm-linux-androideabi-'
        when /arm64-v8a/              then 'aarch64-linux-android-'
        when /x86/                    then 'x86-'
        when /x86_64/                 then 'x86_64-'
        when /mips/                   then 'mipsel-linux-android-'
        when /mips64/                 then 'mips64el-linux-android-'
        end

      test = case arch
        when /armeabi/, /armeabi-v7a/ then 'arm-linux-androideabi-*'
        when /arm64-v8a/              then 'aarch64-linux-android-*'
        when /x86/                    then 'x86-*'
        when /x86_64/                 then 'x86_64-*'
        when /mips/                   then 'mipsel-linux-android-*'
        when /mips64/                 then 'mips64el-linux-android-*'
        end

      gcc_toolchain_version = Dir[home_path.join('toolchains', test)].map{|t| t.match(/-(\d+\.\d+)$/); $1.to_f }.max
      gcc_toolchain_path = home_path.join('toolchains', prefix + gcc_toolchain_version.to_s, 'prebuilt', host_platform)
      gcc_toolchain_path.join('bin', command).to_s
    end
  end

  def bin(command)
    command = command.to_s
    toolchain_path.join('bin', command).to_s
  end

  def home_path
    @home_path ||= Pathname(
      params[:ndk_home] ||
      ENV['ANDROID_NDK_HOME'] ||
      DEFAULT_NDK_HOMES.find { |path|
        path.gsub! '%LOCALAPPDATA%', ENV['LOCALAPPDATA'] || '%LOCALAPPDATA%'
        path.gsub! '\\', '/'
        File.directory?(path)
      } || raise(AndroidNDKHomeNotFound)
    )
  end

  def toolchain
    @toolchain ||= params.fetch(:toolchain){ DEFAULT_TOOLCHAIN }
  end

  def toolchain_path
    @toolchain_path ||= case toolchain
      when :clang
        home_path.join('toolchains', 'llvm' , 'prebuilt', host_platform)
      end
  end

  def host_platform
    @host_platform ||= case RUBY_PLATFORM
      when /cygwin|mswin|mingw|bccwin|wince|emx/i
        path = home_path.join('toolchains', 'llvm' , 'prebuilt', 'windows*')
        Dir.glob(path.to_s){ |item|
          next if File.file?(item)
          path = Pathname(item)
          break
        }
        path.basename
      when /x86_64-darwin/i
        'darwin-x86_64'
      when /darwin/i
        'darwin-x86'
      when /x86_64-linux/i
        'linux-x86_64'
      when /linux/i
        'linux-x86'
      else
        raise NotImplementedError, "Unknown host platform (#{RUBY_PLATFORM})"
      end
  end

  def arch
    @arch ||= (params[:arch] || ENV['ANDROID_ARCH'] || DEFAULT_ARCH).to_s
  end

  def sysroot
    @sysroot ||= home_path.join('platforms', platform,
        case arch
        when /armeabi/, /armeabi-v7a/ then 'arch-arm'
        when /arm64-v8a/              then 'arch-arm64'
        when /x86/                    then 'arch-x86'
        when /x86_64/                 then 'arch-x86_64'
        when /mips/                   then 'arch-mips'
        when /mips64/                 then 'arch-mips64'
        end
      ).to_s
  end

  def platform
    if @platform === nil then
      @platform = params[:platform] || ENV['ANDROID_PLATFORM'] || nil
      if @platform === nil
        Dir.glob(home_path.join('platforms/android-*').to_s){ |item|
          next if File.file?(item)
          if @platform === nil
            @platform = Integer(item.rpartition('-')[2])
          else
            platform = Integer(item.rpartition('-')[2])
            @platform = platform > @platform ? platform : @platform
          end
        }
        if @platform === nil
          raise(PlatformDirNotFound)
        else
          @platform = "android-#{@platform}"
        end
      end
    end
    if Integer(@platform.rpartition('-')[2]) < 21
      case arch
      when /arm64-v8a/, /x86_64/, /mips64/
        raise NotImplementedError, "Platform (#{@platform}) has no implementation for architecture (#{arch})"
      end
    end
    @platform
  end

  def cc
    case toolchain
    when :clang then bin('clang')
    end
  end

  def ar
    case toolchain
    when :clang then bin_gcc('ar')
    end
  end

  def cflags
    flags = []

    flags += %W(-D__android__ --sysroot="#{sysroot}")
    case toolchain
    when :clang
      flags += %W(-gcc-toolchain #{toolchain_path.to_s})
      case arch
      when /armeabi/      then flags += %W(-target armv5te-none-linux-androideabi)
      when /armeabi-v7a/  then flags += %W(-target armv7-none-linux-androideabi)
      when /arm64-v8a/    then flags += %W(-target aarch64-none-linux-android)
      when /x86/          then flags += %W(-target i686-none-linux-android)
      when /x86_64/       then flags += %W(-target x86_64-none-linux-android)
      when /mips/         then flags += %W(-target mipsel-none-linux-android)
      when /mips64/       then flags += %W(-target mips64el-none-linux-android)
      end
    end

    flags
  end

end

MRuby::Toolchain.new(:android) do |conf, params|
  android = MRuby::Toolchain::Android.new(params)

  toolchain android.toolchain

  [conf.cc, conf.cxx, conf.objc, conf.asm].each do |cc|
    cc.command = android.cc
    cc.flags = android.cflags
  end

  conf.archiver.command = android.ar
  conf.linker.command = android.cc
  conf.linker.flags = []
end
