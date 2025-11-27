MRuby::Gem::Specification.new('mruby-task') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'Cooperative multitasking with preemptive scheduling'

  # Enable task scheduler globally (required for vm.c integration)
  spec.build.defines << 'MRB_USE_TASK_SCHEDULER'

  # Check if HAL gem is loaded
  # HAL gems must be explicitly specified in build config (recommended) or via auto-selection below
  spec.build.gems.one? { |g| g.name =~ /^hal-.*-task$/ } or begin
    # No HAL found - determine appropriate error message or auto-load
    suggested_hal = if spec.for_windows?
      # Windows (including MinGW) - use Windows HAL
      'hal-win-task'
    elsif RUBY_PLATFORM =~ /linux|darwin|bsd/
      'hal-posix-task'
    else
      nil
    end

    if suggested_hal
      # Auto-load HAL gem for convenience (for development)
      # This works because HAL gems declare dependency on mruby-task
      warn "mruby-task: No HAL specified, loading #{suggested_hal} (explicit selection recommended)"
      spec.build.gem core: suggested_hal
    else
      # Unknown platform - fail with helpful message
      fail "mruby-task: No HAL available for platform '#{RUBY_PLATFORM}'.\n" \
           "Please specify HAL gem explicitly in your build config:\n" \
           "  conf.gem core: 'hal-posix-task'   # For Linux/macOS/BSD\n" \
           "  conf.gem core: 'hal-win-task'     # For Windows\n" \
           "Or create custom HAL - see mrbgems/mruby-task/README.md"
    end
  end
end
