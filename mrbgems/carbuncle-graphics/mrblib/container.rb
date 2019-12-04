module Carbuncle
  class Container
    attr_reader :children

    def initialize
      @children = []
    end

    def update(dt)
      children.each { |c| c.update(dt) }
    end

    def draw
      children.each(&:draw)
    end
  end
end
