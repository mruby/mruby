##
# Kernel#sprintf Kernel#format Test

assert("String#%") do
  ("%02x, %3d" % [10, 100]) == "0a, 100" and ("%02X" % 10) == "0A"
end
