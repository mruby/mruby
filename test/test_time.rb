
$ok     = 0
$failed = 0

def assert(str = "Assertion failed") 
    if(!yield) 
      puts("Test Failed: #{str}");
      $failed += 1
    else
      $ok     += 1
    end
end

def report()
  $total = $ok + $failed
  puts "Ran #{$total} tests: #{$ok} OK and #{$failed} failed."
end

doom = Time.gm(2012, 12, 23)
assert("Time.gm")   { doom                      }
p doom.asctime
assert("gm year")   { doom.year == 2012         }
assert("gm month")  { doom.month == 12          }
assert("gm day")    { doom.mday == 23           }

p doom.year 
p doom.month 
p doom.mday

t0 = Time.new
# p(t0.utc?)
# assert { false }
assert("Can create time.") { t0 }

t1 = Time.new(1300000000.0e6)
p t1.asctime
p t1.year
assert("asctime") { t1.asctime == "Sun Mar 13 08:06:40 2011\n" }
assert("usec")    { t1.usec    ==  1300000000.0e6 }
assert("to_i")    { t1.to_i    ==  1300000000     }
assert("to_f")    { t1.to_f    ==  1300000000.0   }
assert("utc?")    { !(t1.utc?)                    }
assert("zone")    { t1.zone    == "LOCAL"         }
assert("wday")    { t1.wday    ==  0              }
assert("yday")    { t1.yday    ==  71             }
assert("year")    { t1.year    ==  2011           }

t2 = Time.new(7.0e6)
t1.initialize_copy(t2)
assert("initialize_copy") { t1 == t2 }







report()
