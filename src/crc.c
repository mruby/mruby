/*
** crc.c - calculate CRC
**
** See Copyright Notice in mruby.h
*/

#include <limits.h>
#include <stdint.h>
#include <sys/types.h>

// Calculate CRC (CRC-16-CCITT)
//
//  0000_0000_0000_0000_0000_0000_0000_0000
//          ^|------- CRC -------|- work --|
//        carry
#define  CRC_16_CCITT       0x11021ul        //x^16+x^12+x^5+1
#define  CRC_XOR_PATTERN    (CRC_16_CCITT << 8)
#define  CRC_CARRY_BIT      (1 << 24)

uint16_t
calc_crc_16_ccitt(const uint8_t *src, uint32_t nbytes, uint16_t crc)
{
  uint32_t ibyte, ibit;
  uint32_t crcwk = crc << 8;

  for (ibyte = 0; ibyte < nbytes; ibyte++) {
    crcwk |= *src++;
    for (ibit = 0; ibit < CHAR_BIT; ibit++) {
      crcwk <<= 1;
      if (crcwk & CRC_CARRY_BIT) {
        crcwk ^= CRC_XOR_PATTERN;
      }
    }
  }
  return (uint16_t)(crcwk >> 8);
}

