class String
  def underscore
    gsub(/::/, '/')
      .gsub(/([A-Z]+)([A-Z][a-z])/, '\1_\2')
      .gsub(/([a-z\d])([A-Z])/, '\1_\2')
      .tr('-', '_')
      .tr(' ', '_')
      .downcase
  end

  def strip_heredoc
    gsub(/^#{scan(/^[ \t]*(?=\S)/).min}/, ''.freeze)
  end
end
