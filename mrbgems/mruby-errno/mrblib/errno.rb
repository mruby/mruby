module Errno
  #
  # call-seq:
  #   Errno.const_defined?(name) -> true or false
  #
  # Returns true if the given name is defined as an errno constant,
  # false otherwise. This method checks both system-defined errno
  # constants and those defined by the superclass.
  #
  #   Errno.const_defined?(:ENOENT)  #=> true
  #   Errno.const_defined?(:EINVAL)  #=> true
  #   Errno.const_defined?(:UNKNOWN) #=> false
  #
  def Errno.const_defined?(name)
    __errno_defined?(name) or super
  end

  #
  # call-seq:
  #   Errno.const_missing(name) -> errno_class
  #
  # Called when an undefined constant is referenced. This method
  # attempts to define the errno constant if it exists in the system,
  # otherwise delegates to the superclass.
  #
  #   Errno::ENOENT  # triggers const_missing if not yet defined
  #   #=> Errno::ENOENT
  #
  def Errno.const_missing(name)
    __errno_define(name) or super
  end

  #
  # call-seq:
  #   Errno.constants -> array
  #
  # Returns an array of all errno constant names available on the system.
  # This includes both already defined constants and those that can be
  # dynamically defined.
  #
  #   Errno.constants
  #   #=> [:EPERM, :ENOENT, :ESRCH, :EINTR, :EIO, ...]
  #
  # Note: Module#constants is defined in mruby-metaprog, so this method
  # may raise NoMethodError if that gem is not available.
  #
  def Errno.constants
    __errno_list(super)
  end
end
