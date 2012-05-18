$ok_test = 0
$ko_test = 0
$kill_test = 0
$asserts  = []

##
# Print the assertion in a readable way
def print_assertion_string(str, iso)
  print(str)
  if(iso != '')
    print(' [')
    print(iso)
    print(']')
  end
end

##
# Verify a code block.
#
# str : A remark which will be printed in case
#       this assertion fails
# iso : The ISO reference code of the feature
#       which will be tested by this
#       assertion
def assert(str = 'Assertion failed', iso = 'No ISO')
  begin
    if(!yield)
      $asserts.push([str, iso])
      $ko_test += 1
      print('F')
    else
      $ok_test += 1
      print('.')
    end
  rescue
    $kill_test += 1
    print('X')
  end
end

##
# Report the test result and print all assertions
# which were reported broken.
def report()
  print "\n"
  $asserts.each do |str, iso|
    print('Fail: ');
    print_assertion_string(str, iso)
    print("\n")
  end

  $total_test = $ok_test.+($ko_test)
  print('Total: ')
  print($total_test)
  print("\n")

  print('   OK: ')
  print($ok_test)
  print("\n")
  print('   KO: ')
  print($ko_test)
  print("\n")
  print('Crash: ')
  print($kill_test)
  print("\n")
end

