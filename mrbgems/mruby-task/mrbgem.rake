MRuby::Gem::Specification.new('mruby-task') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'Cooperative multitasking with preemptive scheduling'

  # Enable task scheduler globally (required for vm.c integration)
  spec.build.defines << 'MRB_USE_TASK_SCHEDULER'

  # Windows: link with multimedia timer library
  if spec.for_windows?
    spec.linker.libraries << 'winmm'
  end

  spec.add_dependency 'mruby-fiber'  # Uses same context infrastructure
end
