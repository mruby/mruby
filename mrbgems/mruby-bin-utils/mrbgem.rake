MRuby::Gem::Specification.new("mruby-bin-utils") do |spec|
  spec.summary = "one-shot mruby C/C++ compiler and utilities"
  spec.license = "MIT"
  spec.author  = "mruby developers"
  spec.add_dependency "mruby-bin-config", core: "mruby-bin-config"

  if ENV["OS"] =~ /windows/i
    msvc = build.toolchains.find { |e| e == "visualcpp" } ? true : false
    binext = ".bat"
    bingen = ->(bin, cflags, ldflags) {
      (cflags, ldflags) = [cflags, ldflags].map { |flags|
        flags.map { |f|
          varname = %(mrbc_#{f.sub(/^--/, "").gsub(/[^\w]+/, "_")})
          extractor = %(for /f "usebackq delims=" %%i in (`%mrbconfig% #{f}`) do set #{varname}=%%i)
          ["%#{varname}%", extractor]
        }
      }

      withlink = "/link " if msvc && bin != "mruby-ld"

      <<~CODE
        @echo off

        if "%1" == "" (
        \techo %~n0: no files given 1>&2
        \texit /b 1
        )

        set mrbconfig="%~dp0.\\mruby-config.bat"
        #{cflags.map { |v, e| e }.join("\n")}
        #{ldflags.map { |v, e| e }.join("\n")}

        call #{cflags.map { |v, e| v }.join(" ")} %* #{withlink}#{ldflags.map { |v, e| v }.join(" ")}
      CODE
    }
  else
    binext = ""
    bingen = ->(bin, cflags, ldflags) {
      <<~CODE
        #!/bin/sh

        if [ "$#" -lt 1 ]
        then
        \techo "$(basename "$0"): no files given" 1>&2
        \texit 1
        fi

        mrbconfig="$(dirname "$0")/mruby-config"

        $("$mrbconfig" #{cflags.join(" ")}) "$@" $("$mrbconfig" #{ldflags.join(" ")})
      CODE
    }
  end

  if spec.build.kind_of?(MRuby::CrossBuild)
    destdir = "#{build.build_dir}/host-bin"
    addbin = ->(dest) { build.products << dest }
  else
    destdir = "#{build.build_dir}/bin"
    addbin = ->(dest) { build.bins << File.basename(dest) }
  end

  [
    ["mruby-cc",    %w(--cc --cflags),      %w(--ldflags --ldflags-before-libs --libs)],
    ["mruby-c++",   %w(--cxx --cxxflags),   %w(--ldflags --ldflags-before-libs --libs)],
    ["mruby-as",    %w(--as --asflags),     %w(--ldflags --ldflags-before-libs --libs)],
    ["mruby-objc",  %w(--objc --objcflags), %w(--ldflags --ldflags-before-libs --libs)],
    ["mruby-ld",    %w(--ld),               %w(--ldflags --ldflags-before-libs --libs)],
  ].each do |bin, cflags, ldflags|
    destpath = File.join(destdir, "#{bin}#{binext}")

    addbin.call(destpath)

    file destpath => __FILE__ do |t|
      _pp "GEN", destpath.relative_path
      mkdir_p destdir

      # NOTE: No line break code is specified, so text appropriate to the environment is output
      File.write(destpath, bingen.call(bin, cflags, ldflags), perm: 0755)
    end
  end
end
