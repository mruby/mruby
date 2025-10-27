MRuby::Gem::Specification.new('mruby-dir') do |spec|
  spec.license = 'MIT and MIT-like license'
  spec.authors = ['Internet Initiative Japan Inc.', 'Kevlin Henney']

  # Check if HAL gem is loaded
  # HAL gems must be explicitly specified in build config (recommended) or via auto-selection below
  spec.build.gems.one? { |g| g.name =~ /^hal-.*-dir$/ } or begin
    # No HAL found - determine appropriate error message or auto-load
    suggested_hal = if ENV['MRUBY_DIR_HAL']
      ENV['MRUBY_DIR_HAL']
    elsif spec.for_windows?
      'hal-win-dir'
    elsif RUBY_PLATFORM =~ /linux|darwin|bsd/
      'hal-posix-dir'
    else
      nil
    end

    if suggested_hal
      # Auto-load HAL gem for convenience (for development)
      # This works because HAL gems declare dependency on mruby-dir
      warn "mruby-dir: No HAL specified, loading #{suggested_hal} (explicit selection recommended)"
      spec.build.gem core: suggested_hal
    else
      # Unknown platform - fail with helpful message
      fail "mruby-dir: No HAL available for platform '#{RUBY_PLATFORM}'.\n" \
           "Please specify HAL gem explicitly in your build config:\n" \
           "  conf.gem core: 'hal-posix-dir'   # For Linux/macOS/BSD\n" \
           "  conf.gem core: 'hal-win-dir'     # For Windows\n" \
           "Or set environment variable:\n" \
           "  MRUBY_DIR_HAL=hal-myplatform-dir\n" \
           "See mrbgems/mruby-dir/README.md for creating custom HAL."
    end
  end
end
