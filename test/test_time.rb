$ok     = 0
$failed = 0
$ftest  = []

def assert(str = "Assertion failed")
    if(!yield)
      $ftest.push(str)
      $failed += 1
      print "F"
    else
      $ok     += 1
      print "."
    end
end

def report()
  print "\n"
  $ftest.each do |str|
    puts("Test Failed: #{str}");
  end
  $total = $ok + $failed
  puts "Ran #{$total} tests: #{$ok} OK and #{$failed} failed."
end

doom = Time.gm(2012, 12, 23)
assert("Time.gm")   { doom                      }
assert("gm year")   { doom.year == 2012         }
assert("gm month")  { doom.month == 12          }
assert("gm day")    { doom.mday == 23           }

t0 = Time.new
assert("Can create time.") { t0 }

t1 = Time.at(1300000000.0).utc
assert("asctime") { t1.asctime == "Sun Mar 13 07:06:40 UTC 2011" }
assert("usec")    { t1.usec    ==  0              }
assert("to_i")    { t1.to_i    ==  1300000000     }
assert("to_f")    { t1.to_f    ==  1300000000.0   }
assert("utc?")    { t1.utc?                       }
assert("zone")    { t1.zone    == "UTC"           }
assert("wday")    { t1.wday    ==  0              }
assert("yday")    { t1.yday    ==  71             }
assert("year")    { t1.year    ==  2011           }

t2 = Time.at(7.0e6)
assert("initialize_copy") { t2.clone == t2 }

report()
