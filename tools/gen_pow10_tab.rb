#!/usr/bin/env ruby
# frozen_string_literal: true

# Generate pow10 table for mruby unrounded scaling (fp_uscale.c)
#
# Uses exact integer arithmetic only.
# For each p in [-343, 341], computes (hi, lo) such that:
#   10^p ~= (hi * 2^64 - lo) * 2^pe
# where pe = floor(p * log2(10)) - 127
#
# The 128-bit value pm = hi * 2^64 - lo is in [2^127, 2^128).

POW10_MIN = -343
POW10_MAX = 341

def generate_entry(p)
  if p >= 0
    val = 10**p
    bit_len = val.bit_length
    pe = bit_len - 128
    if pe >= 0
      mask = (1 << pe) - 1
      pm = (val >> pe) + ((val & mask) != 0 ? 1 : 0)
    else
      pm = val << (-pe)
    end
  else
    abs_p = -p
    denom = 10**abs_p
    # pe = floor(p * log2(10)) - 127
    # Ruby's integer division of negative numbers does floor division
    pe_est = (p * 108853 >> 15) - 127

    numerator = 1 << (-pe_est)
    pm = (numerator + denom - 1) / denom

    # Adjust pe if pm is out of range [2^127, 2^128)
    while pm >= (1 << 128)
      pe_est += 1
      numerator = 1 << (-pe_est)
      pm = (numerator + denom - 1) / denom
    end
    while pm < (1 << 127)
      pe_est -= 1
      numerator = 1 << (-pe_est)
      pm = (numerator + denom - 1) / denom
    end
  end

  raise "pm out of range for p=#{p}: #{pm.bit_length}" unless pm.bit_length == 128

  hi = (pm >> 64) + ((pm & ((1 << 64) - 1)) != 0 ? 1 : 0)
  lo = (hi << 64) - pm

  raise "hi out of range for p=#{p}" unless hi >= (1 << 63) && hi < (1 << 64)

  { hi: hi, lo: lo }
end

def main
  entries = (POW10_MIN..POW10_MAX).map { |p| [p, generate_entry(p)] }

  entries.each do |p, e|
    printf("  {0x%016xULL, 0x%016xULL},\n", e[:hi], e[:lo])
  end
end

main
