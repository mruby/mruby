MRuby::Gem::Specification.new('mruby-socket') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Internet Initiative Japan Inc.', 'mruby developers']
  spec.summary = 'standard socket class'

  #spec.cc.defines << "HAVE_SA_LEN=0"

  spec.add_dependency('mruby-io', :core => 'mruby-io')
  spec.add_dependency('mruby-error', :core => 'mruby-error')
  # spec.add_dependency('mruby-mtest')

  # Check if HAL gem is loaded
  # HAL gems must be explicitly specified in build config (recommended) or via auto-selection below
  spec.build.gems.one? { |g| g.name =~ /^hal-.*-socket$/ } or begin
    # No HAL found - determine appropriate error message or auto-load
    suggested_hal = if spec.for_windows?
      'hal-win-socket'
    elsif RUBY_PLATFORM =~ /linux|darwin|bsd/
      'hal-posix-socket'
    else
      nil
    end

    if suggested_hal
      # Auto-load HAL gem for convenience (for development)
      # This works because HAL gems declare dependency on mruby-socket
      warn "mruby-socket: No HAL specified, loading #{suggested_hal} (explicit selection recommended)"
      spec.build.gem core: suggested_hal
    else
      # Unknown platform - fail with helpful message
      fail "mruby-socket: No HAL available for platform '#{RUBY_PLATFORM}'.\n" \
           "Please specify HAL gem explicitly in your build config:\n" \
           "  conf.gem core: 'hal-posix-socket'   # For Linux/macOS/BSD\n" \
           "  conf.gem core: 'hal-win-socket'     # For Windows"
    end
  end
end
