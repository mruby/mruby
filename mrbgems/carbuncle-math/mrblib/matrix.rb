module Carbuncle
  class Matrix
    include Enumerable

    class Line
      include Enumerable

      attr_reader :matrix

      def initialize(matrix, index, vertical)
        @matrix = matrix
        @index = index
        @indexes =
          Array.new(4) do |v|
            vertical ? [index, v] : [v, index]
          end
      end

      def [](index)
        i, j = @indexes[index]
        matrix[i, j]
      end

      def []=(index, value)
        i, j = @indexes[index]
        matrix[i, j] = value
      end

      def to_a
        @indexes.map do |indexes|
          i, j = indexes
          matrix[i, j]
        end
      end

      def each(&block)
        to_a.each(&block)
      end

      def to_s
        inspect
      end

      def inspect
        "#{self.class.name}[#{@index}](#{to_a.join(', ')})"
      end

      def size
        4
      end
    end

    class Row < Carbuncle::Matrix::Line
      def initialize(matrix, i)
        super(matrix, i, false)
      end
    end

    class Column < Carbuncle::Matrix::Line
      def initialize(matrix, j)
        super(matrix, j, true)
      end
    end

    def each(&block)
      rows.map(&:to_a).flatten.each(&block)
    end

    def rows
      @rows ||= [
        Carbuncle::Matrix::Row.new(self, 0),
        Carbuncle::Matrix::Row.new(self, 1),
        Carbuncle::Matrix::Row.new(self, 2),
        Carbuncle::Matrix::Row.new(self, 3)
      ]
    end

    def columns
      @columns ||= [
        Carbuncle::Matrix::Column.new(self, 0),
        Carbuncle::Matrix::Column.new(self, 1),
        Carbuncle::Matrix::Column.new(self, 2),
        Carbuncle::Matrix::Column.new(self, 3)
      ]
    end

    def to_s
      inspect
    end

    def inspect
      result = <<-HEREDOC
        Matrix:
          #{rows[0].to_a.join(', ')},
          #{rows[1].to_a.join(', ')},
          #{rows[2].to_a.join(', ')},
          #{rows[3].to_a.join(', ')},
      HEREDOC
      result.strip_heredoc
    end

    def size
      16
    end
  end
end
