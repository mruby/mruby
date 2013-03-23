module MRuby
  module LoadGems
    def gem(gemdir, &block)
      caller_dir = File.expand_path(File.dirname(/^(.*?):\d/.match(caller.first).to_a[1]))
      if gemdir.is_a?(Hash)
        gemdir = load_external_gem(gemdir)
      else
        gemdir = File.expand_path(gemdir, caller_dir)
      end
      gemrake = File.join(gemdir, "mrbgem.rake")

      fail "Can't find #{gemrake}" unless File.exists?(gemrake)
      Gem.current = nil
      load gemrake
      return nil unless Gem.current

      Gem.current.dir = gemdir
      Gem.current.build = MRuby::Build.current
      Gem.current.build_config_initializer = block
      gems << Gem.current
      Gem.current
    end

    def load_external_gem(params)
      if params[:github]
        params[:git] = "https://github.com/#{params[:github]}.git"
      elsif params[:bitbucket]
        params[:git] = "https://bitbucket.org/#{params[:bitbucket]}.git"
      end

      if params[:git]
        url = params[:git]
        gemdir = "build/mrbgems/#{url.match(/([-_\w]+)(\.[-_\w]+|)$/).to_a[1]}"
        return gemdir if File.exists?(gemdir)

        options = [params[:options]] || []
        options << "--branch \"#{params[:branch]}\"" if params[:branch]

        FileUtils.mkdir_p "build/mrbgems"
        git.run_clone gemdir, url, options
        
        gemdir
      else
        fail "unknown gem option #{params}"
      end
    end

    def enable_gems?
      !@gems.empty?
    end
  end # LoadGems
end # MRuby
