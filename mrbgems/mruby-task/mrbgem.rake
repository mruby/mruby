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
end
