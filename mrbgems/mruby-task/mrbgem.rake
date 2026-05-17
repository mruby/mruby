MRuby::Gem::Specification.new('mruby-task') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'Cooperative multitasking with preemptive scheduling'

  # Enable task scheduler globally (required for vm.c integration)
  spec.build.defines << 'MRB_USE_TASK_SCHEDULER'

  # A gem named hal-<name>-task (declaring add_dependency 'mruby-task')
  # replaces the built-in ports/<conf.ports>/ HAL implementation.
  spec.hal_pattern = /\Ahal-.*-task\z/

  if spec.for_windows?
    spec.linker.libraries << "winmm"
  end

  ports = spec.build.effective_ports

  # ports/glib/ needs glib-2.0 (GSource, GMainContext, GRecMutex) and
  # gthread-2.0 (GThread). On modern distros gthread-2.0 is a transparent
  # alias for glib-2.0; on older ones it's a separate .pc that pulls in
  # -lpthread, so query it separately.
  if ports.include?('glib')
    unless spec.search_package('glib-2.0') && spec.search_package('gthread-2.0')
      abort <<~MSG
        [mruby-task] conf.ports :glib selected but pkg-config could not find
        glib-2.0 / gthread-2.0. Install the GLib development headers
        (Debian/Ubuntu: libglib2.0-dev; Fedora: glib2-devel; Arch: glib2;
        macOS Homebrew: glib). For non-default install locations, set
        PKG_CONFIG_PATH before invoking rake.
      MSG
    end
  end

  # Optional demo tool that exercises the GLib HAL end-to-end (basic
  # scheduling, priority ordering, timeslice preemption, auto-execution
  # under a foreign GLib main loop). Default off; opt in from your
  # build_config with:
  #
  #     conf.cc.defines << 'MRB_TASK_BUILD_DEMO'
  #     conf.ports :glib
  #     conf.gem core: 'mruby-task'
  #
  # When enabled, `rake` produces bin/mruby-task-demo from
  # tools/mruby-task-demo/. The define is only inspected here -- the
  # demo's C source does not condition on it.
  if spec.build.cc.defines.include?('MRB_TASK_BUILD_DEMO')
    unless ports.include?('glib')
      abort '[mruby-task] MRB_TASK_BUILD_DEMO requires conf.ports :glib'
    end
    spec.bins = %w(mruby_task_demo)
  end
end
