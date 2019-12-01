class Carbuncle::Point
  %w[x y].permutation(2) do |permutation|
    eval <<-HEREDOC
      def #{permutation.join}
        [#{permutation.join(',')}]
      end

      def #{permutation.join}=(value)
        #{permutation.map { |p| "self.#{p} = value\n" }}
        value
      end
    HEREDOC
  end
end
