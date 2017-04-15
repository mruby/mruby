##
# Issues Test

assert("Issue#547") do 
  module Test4Issue547
    def self.test
      1.times do
        1.times do
          return :ok
        end
      end
      :bad
    end
  end
  Test4Issue547.test == :ok    
end
