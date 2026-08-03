# CRuby defines these on Symbol so that a symbol can be matched against a
# regexp without spelling out the `to_s`.  They are the String methods applied
# to the symbol's name, so delegate instead of repeating the pattern handling;
# `$~` is set by the engine either way.
#
# This covers the symbol-on-the-left direction only.  The Regexp side converts
# a symbol on its own, in `match_operand()` in regexp.c, so it needs nothing
# from here.  `sym[/re/]` needs nothing either: Symbol#slice (mruby-symbol-ext,
# which this gem does not depend on) delegates to String#slice, so it picks up
# the regexp form from string_regexp.rb wherever that gem is built in.
#
# The argument handling of `=~` is inherited from `String#=~` rather than
# introduced here, and agrees with CRuby: a String argument raises TypeError,
# and any other object is asked for its own `=~`, which `nil` answers and an
# object without one rejects with NoMethodError.
class Symbol
  def match(re, pos = 0, &block)
    self.to_s.match(re, pos, &block)
  end

  def match?(re, pos = 0)
    self.to_s.match?(re, pos)
  end

  def =~(re)
    self.to_s =~ re
  end
end
