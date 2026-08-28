MRuby::Gem::Specification.new('mruby-process') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'Process module and Process::Status class'

  # `Process.kill(:TERM, pid)` takes a signal by name and `Process::Status#to_s`
  # spells one out, so both need the platform's signal table.  mruby-signal owns
  # it; this gem reaches it through signal_hal.h rather than keeping a copy.
  spec.add_dependency 'mruby-signal', core: 'mruby-signal'

  # mruby-process needs no I/O of its own.  The tests do: waiting on a child
  # is only testable with a child, and IO.popen is how one is made.  The
  # dependency stops at the tests; see README.md.
  spec.add_test_dependency 'mruby-io', core: 'mruby-io'

  # A gem's tests run in a state holding its dependency closure and nothing
  # else, so a test that means to name an Errno class has to ask for the gem
  # that defines them.  Without this the tests still pass, by taking the
  # branch that settles for any StandardError.
  spec.add_test_dependency 'mruby-errno', core: 'mruby-errno'

  # Asking a Process::Status what instance_variables it hands out needs the
  # gem that defines Object#instance_variables in the first place.
  spec.add_test_dependency 'mruby-metaprog', core: 'mruby-metaprog'
end
