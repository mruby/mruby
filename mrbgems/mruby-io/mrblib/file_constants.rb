class File
  # File name matching constants used with File.fnmatch and Dir.glob
  module Constants
    # Makes File.fnmatch case sensitive on systems where it's case insensitive by default
    FNM_SYSCASE  = 0

    # Disables the special meaning of the backslash escape character
    FNM_NOESCAPE = 1

    # Pathname wildcard doesn't match '/' (directory separator)
    FNM_PATHNAME = 2

    # Allows patterns to match hidden files (those starting with '.')
    FNM_DOTMATCH = 4

    # Makes the pattern case insensitive (overrides FNM_SYSCASE)
    FNM_CASEFOLD = 8
  end
end

class File
  # Include Constants module to make FNM_* constants available directly on File class
  include File::Constants
end
