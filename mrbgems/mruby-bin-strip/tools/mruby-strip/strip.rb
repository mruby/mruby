def print_usage_exit msg = nil
  $stderr.write msg if msg
  $stderr.write "Usage: #{$0} [switches] irepfiles\n"
  $stderr.write "switches:\n"
  $stderr.write "  -l, --lvar   remove LVAR section too.\n"
  exit 1
end

print_usage_exit "no files to strip\n" if ARGV.empty?

$remove_lvar = false
targets = []

ARGV.each do |v|
  case v
  when '-l', '--lvar'
    $remove_lvar = true
    next
  else
    print_usage_exit "invalid flag: #{v}" if v[0] == '-'
  end

  targets << v
end

targets.each do |fname|
  iseq = nil

  # load
  begin
    iseq = MRubyVM::InstructionSequence.load_from_binary File.read(fname, mode: 'rb')
    iseq.remove_lvar if $remove_lvar
  rescue ScriptError
    $stderr.write "can't read irep file #{fname}\n"
    exit 1
  rescue
    $stderr.write "can't open file for reading #{fname}\n"
    exit 1
  end

  # re-dump
  File.open fname, 'wb' do |f|
    f.write iseq.to_binary mrb_debug_info: false
  end
end
