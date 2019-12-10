module Carbuncle
  class Matrix
    include Enumerable

    def each(&block)
      [
        self[0, 0], self[0, 1], self[0, 2], self[0, 3],
        self[1, 0], self[1, 1], self[1, 2], self[1, 3],
        self[2, 0], self[2, 1], self[2, 2], self[2, 3],
        self[3, 0], self[3, 1], self[3, 2], self[3, 3]
      ].each(&block)
    end
  end
end
