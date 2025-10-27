MRuby::Gem::Specification.new('mruby-io') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Internet Initiative Japan Inc.', 'mruby developers']
  spec.summary = 'IO and File class'

  spec.build.defines << "HAVE_MRUBY_IO_GEM"
  spec.add_test_dependency 'mruby-time', core: 'mruby-time'

  # Check if HAL gem is loaded
  # HAL gems must be explicitly specified in build config (recommended) or via auto-selection below
  spec.build.gems.one? { |g| g.name =~ /^hal-.*-io$/ } or begin
    # No HAL found - determine appropriate error message or auto-load
    suggested_hal = if spec.for_windows?
      'hal-win-io'
    elsif RUBY_PLATFORM =~ /linux|darwin|bsd/
      'hal-posix-io'
    else
      nil
    end

    if suggested_hal
      # Auto-load HAL gem for convenience (for development)
      # This works because HAL gems declare dependency on mruby-io
      warn "mruby-io: No HAL specified, loading #{suggested_hal} (explicit selection recommended)"
      spec.build.gem core: suggested_hal
    else
      # Unknown platform - fail with helpful message
      fail "mruby-io: No HAL available for platform '#{RUBY_PLATFORM}'.\n" \
           "Please specify HAL gem explicitly in your build config:\n" \
           "  conf.gem core: 'hal-posix-io'   # For Linux/macOS/BSD\n" \
           "  conf.gem core: 'hal-win-io'     # For Windows"
    end
  end
end
