require "yaml"
require "pathname"

MRuby.each_target do |build|
  license_file = "#{build_dir}/LICENSE.yml"
  build.products << license_file

  task "license" => license_file

  coreterms = ["LICENSE"]

  termfiles = []
  termfiles << coreterms.fileset_list_under(MRUBY_ROOT)
  termfiles << build.terms.fileset_list_under(build.build_dir)
  build.gems.each { |g|
    termfiles << File.join(g.dir, "mrbgem.rake")
    termfiles << g.terms.fileset_list_under(g.dir)
  }
  termfiles.flatten!
  termfiles.compact!

  file license_file => [__FILE__, MRuby::Build.mruby_config_path, *termfiles] do |t|
    _pp "GEN", t.name.relative_path

    info = {
      "DISCLAIMER FOR THIS FILE" => <<~DISCLAIMER,
        Please note that this file may NOT completely cover the license
        terms of the copyrighted work generated based on the mruby build
        configuration file.

        If you suspect a shortage, please check if it is provided as a
        separate file and contact the software provider.
      DISCLAIMER
      "mruby" => {
        "AUTHORS" => "mruby developers",
        "LICENSES" => "MIT",
        "TERMS" => coreterms.fileset_make_under(MRUBY_ROOT, prefix: "mruby")
      }
    }

    addterms = build.terms.fileset_make_under(build.build_dir, MRUBY_ROOT, prefix: "additional")
    info["additional terms by build_config"] = addterms if addterms

    if enable_gems?
      gems_info = []

      build.gems.each do |g|
        gemterms = g.terms.fileset_make_under(g.dir, g.build_dir, MRUBY_ROOT, prefix: g.name)

        unless gemterms
          if g.core?
            gemterms = "same as mruby's license"
          else
            $stderr.puts "WARNING: no such LICENSE files for #{Rake.verbose ? g.dir : g.name}"
            gemterms = "<<<FOR THE DETAILED LICENSE TERMS OF THIS GEM, PLEASE CHECK DIRECTLY>>>"
          end
        end

        gems_info << {
          "NAME" => g.name,
          "AUTHORS" => Array(g.authors).flatten.uniq,
          "LICENSES" => Array(g.licenses).flatten.uniq,
          "TERMS" => gemterms
        }
      end

      info["mruby gems"] = gems_info unless gems_info.empty?
    end

    mkdir_p File.dirname(t.name)
    File.binwrite(t.name, YAML.dump(info))
  end
end

desc "generate LICENSE.yml file for each targets"
task "license"
