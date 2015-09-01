require "pathname"

module MRuby
  module Source
    def self.path
      Pathname.new(File.expand_path('../../../../',__FILE__))
    end
  end
end
