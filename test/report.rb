report
if self.methods.include?(:exit)
  exit ($ko_test == 0 and $kill_test == 0)
end
