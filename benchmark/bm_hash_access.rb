# Repeated lookups of the same keys in the same hashes. Every `[]` and `[]=`
# here searches its entry from scratch, so the phases isolate the two halves
# of that search: hashing the key, which walks a string but not a symbol, and
# probing for it, which does the same work either way. The last phase pays
# both twice, once for the read and once for the write.

REPEAT = 30_000

STR_KEYS = (0...64).map { |i| "key#{i}" }
SYM_KEYS = STR_KEYS.map { |s| s.to_sym }

str_hash = {}
STR_KEYS.each_with_index { |k, i| str_hash[k] = i }

sym_hash = {}
SYM_KEYS.each_with_index { |k, i| sym_hash[k] = i }

counts = {}
STR_KEYS.each { |k| counts[k] = 0 }

sum = 0

REPEAT.times { STR_KEYS.each { |k| sum += str_hash[k] } }
REPEAT.times { SYM_KEYS.each { |k| sum += sym_hash[k] } }
REPEAT.times { STR_KEYS.each { |k| counts[k] += 1 } }

raise "unexpected sum" unless sum == 2 * REPEAT * 2016
raise "unexpected count" unless counts[STR_KEYS[0]] == REPEAT
