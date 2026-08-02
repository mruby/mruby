# CRuby defines these on Symbol so that a symbol can be matched against a
# regexp without spelling out the `to_s`.  They are the String methods applied
# to the symbol's name, so delegate instead of repeating the pattern handling;
# `$~` is set by the engine either way.
#
# This covers the symbol-on-the-left direction only.  The Regexp side still
# rejects symbols -- `Regexp#=~`, `#match` and `#match?` raise TypeError and
# `#===` returns false -- so `/^to_/ =~ :to_s` and `syms.grep(/^to_/)` (which
# goes through `Regexp#===`) do not work yet.  That is a separate fix in
# regexp.c, as is `sym[/re/]`, which needs the regexp form of `String#slice`.
#
# Two differences from CRuby are inherited from `String#=~` rather than
# introduced here: a String argument raises TypeError (CRuby does too), but so
# does any other object that has no `=~` -- `:a =~ nil` raises NoMethodError
# where CRuby returns nil.
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
