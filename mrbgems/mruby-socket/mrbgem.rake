MRuby::Gem::Specification.new('mruby-socket') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Internet Initiative Japan Inc.', 'mruby developers']
  spec.summary = 'standard socket class'

  #spec.cc.defines << "HAVE_SA_LEN=0"

  spec.add_dependency('mruby-io', :core => 'mruby-io')
  spec.add_dependency('mruby-error', :core => 'mruby-error')
  # spec.add_dependency('mruby-mtest')

  # The tests need to tell one socket() failure from another - notably
  # EAFNOSUPPORT, which is what a host without IPv6 answers and is a
  # reason to skip a test rather than fail it. Without this gem
  # mrb_sys_fail raises a RuntimeError that spells the errno as a bare
  # number, leaving no class to match on. A test dependency rather
  # than a runtime one: the library itself works without Errno being
  # defined, only the tests need to name it.
  spec.add_test_dependency('mruby-errno', :core => 'mruby-errno')
  # A test reaches what the methods build through ObjectSpace, the way a
  # script can; it skips where the build has none.
  if build.gems.any? {|g| g.name == 'mruby-objectspace'}
    spec.add_test_dependency('mruby-objectspace', :core => 'mruby-objectspace')
  end

  if spec.for_windows?
    spec.linker.libraries << "wsock32"
    spec.linker.libraries << "ws2_32"
    spec.linker.libraries << "iphlpapi"  # for GetAdaptersAddresses (Socket.ip_address_list)
  end
end
