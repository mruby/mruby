/*
 ** pack.c - Array#pack, String#unpack
 */

#include <mruby.h>
#include <mruby/error.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/numeric.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/endian.h>
#include <mruby/presym.h>

#include <ctype.h>
#include <string.h>

#define INT_OVERFLOW_P(n)  ((n) < MRB_INT_MIN || (n) > MRB_INT_MAX)
#define UINT_OVERFLOW_P(n) ((n) > MRB_INT_MAX)

#ifndef EOF
# define EOF (-1) /* for MRB_NO_STDIO */
#endif

struct tmpl {
  mrb_value str;
  int idx;
};

enum pack_dir {
  PACK_DIR_CHAR,      /* C */
  PACK_DIR_SHORT,     /* S */
  PACK_DIR_LONG,      /* L */
  PACK_DIR_QUAD,      /* Q */
  //PACK_DIR_INT,     /* i */
  //PACK_DIR_VAX,
  PACK_DIR_BER,       /* w */
  PACK_DIR_UTF8,      /* U */
  PACK_DIR_DOUBLE,    /* E */
  PACK_DIR_FLOAT,     /* f */
  PACK_DIR_STR,       /* A */
  PACK_DIR_HEX,       /* h */
  PACK_DIR_BSTR,      /* b */
  PACK_DIR_BASE64,    /* m */
  PACK_DIR_UU,        /* u */
  PACK_DIR_QENC,      /* M */
  PACK_DIR_NUL,       /* x */
  PACK_DIR_BACK,      /* X */
  PACK_DIR_ABS,       /* @ */
  PACK_DIR_NONE,      /* - */
};

enum pack_type {
  PACK_TYPE_INTEGER,
  PACK_TYPE_FLOAT,
  PACK_TYPE_STRING,
  PACK_TYPE_NONE
};

#define PACK_FLAG_s             0x00000001      /* native size ("_" "!") */
#define PACK_FLAG_a             0x00000002      /* null padding ("a") */
#define PACK_FLAG_Z             0x00000004      /* append nul char ("z") */
#define PACK_FLAG_SIGNED        0x00000008      /* native size ("_" "!") */
#define PACK_FLAG_GT            0x00000010      /* big endian (">") */
#define PACK_FLAG_LT            0x00000020      /* little endian ("<") */
#define PACK_FLAG_WIDTH         0x00000040      /* "count" is "width" */
#define PACK_FLAG_LSB           0x00000080      /* LSB / low nibble first */
#define PACK_FLAG_COUNT2        0x00000100      /* "count" is special... */
#define PACK_FLAG_LITTLEENDIAN  0x00000200      /* little endian actually */

#define PACK_BASE64_IGNORE      0xff
#define PACK_BASE64_PADDING     0xfe
#define IGN                     PACK_BASE64_IGNORE
#define PAD                     PACK_BASE64_PADDING

static const unsigned char base64chars[64] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
  'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
  'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
  'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/',
};
static const unsigned char base64_dec_tab[128] = {
  IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN,
  IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN,
  IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN,  62, IGN, IGN, IGN,  63,
   52,  53,  54,  55,  56,  57,  58,  59,  60,  61, IGN, IGN, IGN, PAD, IGN, IGN,
  IGN,   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,
   15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25, IGN, IGN, IGN, IGN, IGN,
  IGN,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
   41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51, IGN, IGN, IGN, IGN, IGN,
};

/* lookup table for hex character to integer conversion */
static const signed char hex_lookup[256] = {
  /* 0x00-0x0F */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x10-0x1F */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x20-0x2F */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x30-0x3F */  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
  /* 0x40-0x4F */ -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x50-0x5F */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x60-0x6F */ -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x70-0x7F */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x80-0x8F */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x90-0x9F */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xA0-0xAF */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xB0-0xBF */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xC0-0xCF */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xD0-0xDF */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xE0-0xEF */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xF0-0xFF */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
};
/* byte index arrays for endianness optimization */
static const int be_idx16[2] = {1, 0};          /* big-endian 16-bit: MSB, LSB */
static const int le_idx16[2] = {0, 1};          /* little-endian 16-bit: LSB, MSB */
static const int be_idx32[4] = {3, 2, 1, 0};    /* big-endian 32-bit: MSB...LSB */
static const int le_idx32[4] = {0, 1, 2, 3};    /* little-endian 32-bit: LSB...MSB */
static const int be_idx64[8] = {7, 6, 5, 4, 3, 2, 1, 0};  /* big-endian 64-bit: MSB...LSB */
static const int le_idx64[8] = {0, 1, 2, 3, 4, 5, 6, 7};  /* little-endian 64-bit: LSB...MSB */  /* little-endian 64-bit */
/* lookup tables for binary string optimization */
/* Convert character to bit value (0 or 1) */
static inline uint8_t char_to_bit(unsigned char c) {
  switch (c) {
    case '0': return 0;
    case '1': return 1;
    default: return 0;
  }
}

static const char bit_to_char[2] = {'0', '1'};

/* 8-bit batch processing functions for binary strings */
static inline uint8_t
pack_8_bits_msb(const char *src)
{
  uint8_t result = 0;
  result |= char_to_bit((uint8_t)src[0]) << 7;
  result |= char_to_bit((uint8_t)src[1]) << 6;
  result |= char_to_bit((uint8_t)src[2]) << 5;
  result |= char_to_bit((uint8_t)src[3]) << 4;
  result |= char_to_bit((uint8_t)src[4]) << 3;
  result |= char_to_bit((uint8_t)src[5]) << 2;
  result |= char_to_bit((uint8_t)src[6]) << 1;
  result |= char_to_bit((uint8_t)src[7]);
  return result;
}

static inline uint8_t
pack_8_bits_lsb(const char *src)
{
  uint8_t result = 0;
  result |= char_to_bit((uint8_t)src[0]);
  result |= char_to_bit((uint8_t)src[1]) << 1;
  result |= char_to_bit((uint8_t)src[2]) << 2;
  result |= char_to_bit((uint8_t)src[3]) << 3;
  result |= char_to_bit((uint8_t)src[4]) << 4;
  result |= char_to_bit((uint8_t)src[5]) << 5;
  result |= char_to_bit((uint8_t)src[6]) << 6;
  result |= char_to_bit((uint8_t)src[7]) << 7;
  return result;
}

static inline void
unpack_8_bits_msb(uint8_t byte, char *dst)
{
  dst[0] = bit_to_char[(byte >> 7) & 1];
  dst[1] = bit_to_char[(byte >> 6) & 1];
  dst[2] = bit_to_char[(byte >> 5) & 1];
  dst[3] = bit_to_char[(byte >> 4) & 1];
  dst[4] = bit_to_char[(byte >> 3) & 1];
  dst[5] = bit_to_char[(byte >> 2) & 1];
  dst[6] = bit_to_char[(byte >> 1) & 1];
  dst[7] = bit_to_char[byte & 1];
}

static inline void
unpack_8_bits_lsb(uint8_t byte, char *dst)
{
  dst[0] = bit_to_char[byte & 1];
  dst[1] = bit_to_char[(byte >> 1) & 1];
  dst[2] = bit_to_char[(byte >> 2) & 1];
  dst[3] = bit_to_char[(byte >> 3) & 1];
  dst[4] = bit_to_char[(byte >> 4) & 1];
  dst[5] = bit_to_char[(byte >> 5) & 1];
  dst[6] = bit_to_char[(byte >> 6) & 1];
  dst[7] = bit_to_char[(byte >> 7) & 1];
}

/* character classification for string format optimization */
#define CHAR_NULL    0x01
#define CHAR_SPACE   0x02

/* Character classification function */
static inline uint8_t char_class(unsigned char c) {
  switch (c) {
    case '\0': return CHAR_NULL;
    case ' ':
    case '\t':
    case '\n':
    case '\v':
    case '\f':
    case '\r': return CHAR_SPACE;
    default: return 0;
  }
}
/* UTF-8 optimization lookup tables */
/* UTF-8 sequence length lookup table for non-ASCII bytes (0x80-0xFF) */
/* Index = byte_value - 0x80, so table[0] = info for byte 0x80 */
static const uint8_t utf8_seq_len_high[128] = {
  /* 0x80-0xBF: Invalid start bytes (continuation bytes) - return 0 for error */
  0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,

  /* 0xC0-0xDF: 2-byte sequences */
  2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,

  /* 0xE0-0xEF: 3-byte sequences */
  3,3,3,3,3,3,3,3, 3,3,3,3,3,3,3,3,

  /* 0xF0-0xF7: 4-byte sequences */
  4,4,4,4,4,4,4,4,

  /* 0xF8-0xFF: Invalid start bytes - return 0 for error */
  0,0,0,0,0,0,0,0
};

/* Fast validation for UTF-8 continuation bytes (0x80-0xBF) */
/* Index = byte_value - 0x80 */
static const uint8_t utf8_is_continuation[128] = {
  /* 0x80-0xBF: Valid continuation bytes */
  1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,

  /* 0xC0-0xFF: Invalid continuation bytes */
  0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
};

/* Helper macro for continuation byte validation */
#define IS_UTF8_CONTINUATION(byte) \
  ((byte) >= 0x80 && utf8_is_continuation[(byte) - 0x80])

/* Quoted-printable optimization lookup table */
/* 0=literal, 1=encode, 2=special (newline) */
static const uint8_t qprint_encode_type[256] = {
  /* 0x00-0x1F: Control characters - encode */
  1,1,1,1,1,1,1,1, 1,0,2,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
  /*     \t \n                      */

  /* 0x20-0x3F: Printable ASCII */
  0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,1,0,0,
  /*                                                                 =   */

  /* 0x40-0x5F: Printable ASCII */
  0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,

  /* 0x60-0x7F: Printable ASCII */
  0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,1,
  /*                                                                    DEL*/

  /* 0x80-0xFF: High-bit characters - encode */
  1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1
};

/* UU-encoding optimization lookup tables */
/* UU-encoding uses 6-bit values mapped to ASCII 32-95, but space (32) -> backtick (96) */
static const char uu_encode_table[64] = {
  '`', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
  '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
  'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_'
};

/* UU-decoding lookup table for ASCII 32-127 */
static const int8_t uu_decode_table[96] = {
  /* ASCII 32-47: ` ! " # $ % & ' ( ) * + , - . / */
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
  /* ASCII 48-63: 0 1 2 3 4 5 6 7 8 9 : ; < = > ? */
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
  /* ASCII 64-79: @ A B C D E F G H I J K L M N O */
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
  /* ASCII 80-95: P Q R S T U V W X Y Z [ \ ] ^ _ */
  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
  /* ASCII 96-127: ` a b c... (invalid for UU, but ` = 0 for decoding) */
  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
};

/* template parsing optimization structures */
typedef struct {
  enum pack_dir dir;
  enum pack_type type;
  int size;
  unsigned int base_flags;
} format_info_t;

/* Format character lookup function */
static inline format_info_t get_format_info(unsigned char c) {
  switch (c) {
    case 'A': return (format_info_t){PACK_DIR_STR, PACK_TYPE_STRING, 0, PACK_FLAG_WIDTH | PACK_FLAG_COUNT2};
    case 'a': return (format_info_t){PACK_DIR_STR, PACK_TYPE_STRING, 0, PACK_FLAG_WIDTH | PACK_FLAG_COUNT2 | PACK_FLAG_a};
    case 'B': return (format_info_t){PACK_DIR_BSTR, PACK_TYPE_STRING, 0, PACK_FLAG_COUNT2};
    case 'b': return (format_info_t){PACK_DIR_BSTR, PACK_TYPE_STRING, 0, PACK_FLAG_COUNT2 | PACK_FLAG_LSB};
    case 'C': return (format_info_t){PACK_DIR_CHAR, PACK_TYPE_INTEGER, 1, 0};
    case 'c': return (format_info_t){PACK_DIR_CHAR, PACK_TYPE_INTEGER, 1, PACK_FLAG_SIGNED};
    case 'D': return (format_info_t){PACK_DIR_DOUBLE, PACK_TYPE_FLOAT, 8, PACK_FLAG_SIGNED};
    case 'd': return (format_info_t){PACK_DIR_DOUBLE, PACK_TYPE_FLOAT, 8, PACK_FLAG_SIGNED};
    case 'E': return (format_info_t){PACK_DIR_DOUBLE, PACK_TYPE_FLOAT, 8, PACK_FLAG_SIGNED | PACK_FLAG_LT};
    case 'e': return (format_info_t){PACK_DIR_FLOAT, PACK_TYPE_FLOAT, 4, PACK_FLAG_SIGNED | PACK_FLAG_LT};
    case 'F': return (format_info_t){PACK_DIR_FLOAT, PACK_TYPE_FLOAT, 4, PACK_FLAG_SIGNED};
    case 'f': return (format_info_t){PACK_DIR_FLOAT, PACK_TYPE_FLOAT, 4, PACK_FLAG_SIGNED};
    case 'G': return (format_info_t){PACK_DIR_DOUBLE, PACK_TYPE_FLOAT, 8, PACK_FLAG_SIGNED | PACK_FLAG_GT};
    case 'g': return (format_info_t){PACK_DIR_FLOAT, PACK_TYPE_FLOAT, 4, PACK_FLAG_SIGNED | PACK_FLAG_GT};
    case 'H': return (format_info_t){PACK_DIR_HEX, PACK_TYPE_STRING, 0, PACK_FLAG_COUNT2};
    case 'h': return (format_info_t){PACK_DIR_HEX, PACK_TYPE_STRING, 0, PACK_FLAG_COUNT2 | PACK_FLAG_LSB};
    /* I, i, J, j are handled specially based on sizeof() */
    case 'L': return (format_info_t){PACK_DIR_LONG, PACK_TYPE_INTEGER, 4, 0};
    case 'l': return (format_info_t){PACK_DIR_LONG, PACK_TYPE_INTEGER, 4, PACK_FLAG_SIGNED};
    case 'M': return (format_info_t){PACK_DIR_QENC, PACK_TYPE_STRING, 0, PACK_FLAG_WIDTH | PACK_FLAG_COUNT2};
    case 'm': return (format_info_t){PACK_DIR_BASE64, PACK_TYPE_STRING, 0, PACK_FLAG_WIDTH | PACK_FLAG_COUNT2};
    case 'N': return (format_info_t){PACK_DIR_LONG, PACK_TYPE_INTEGER, 4, PACK_FLAG_GT};
    case 'n': return (format_info_t){PACK_DIR_SHORT, PACK_TYPE_INTEGER, 2, PACK_FLAG_GT};
    case 'Q': return (format_info_t){PACK_DIR_QUAD, PACK_TYPE_INTEGER, 8, 0};
    case 'q': return (format_info_t){PACK_DIR_QUAD, PACK_TYPE_INTEGER, 8, PACK_FLAG_SIGNED};
    case 'S': return (format_info_t){PACK_DIR_SHORT, PACK_TYPE_INTEGER, 2, 0};
    case 's': return (format_info_t){PACK_DIR_SHORT, PACK_TYPE_INTEGER, 2, PACK_FLAG_SIGNED};
    case 'u': return (format_info_t){PACK_DIR_UU, PACK_TYPE_STRING, 0, PACK_FLAG_WIDTH | PACK_FLAG_COUNT2};
    case 'U': return (format_info_t){PACK_DIR_UTF8, PACK_TYPE_INTEGER, 0, 0};
    case 'V': return (format_info_t){PACK_DIR_LONG, PACK_TYPE_INTEGER, 4, PACK_FLAG_LT};
    case 'v': return (format_info_t){PACK_DIR_SHORT, PACK_TYPE_INTEGER, 2, PACK_FLAG_LT};
    case 'w': return (format_info_t){PACK_DIR_BER, PACK_TYPE_INTEGER, 0, PACK_FLAG_SIGNED};
    case 'x': return (format_info_t){PACK_DIR_NUL, PACK_TYPE_NONE, 0, 0};
    case 'X': return (format_info_t){PACK_DIR_BACK, PACK_TYPE_NONE, 0, 0};
    case '@': return (format_info_t){PACK_DIR_ABS, PACK_TYPE_NONE, 0, 0};
    case 'Z': return (format_info_t){PACK_DIR_STR, PACK_TYPE_STRING, 0, PACK_FLAG_WIDTH | PACK_FLAG_COUNT2 | PACK_FLAG_Z};
    default: return (format_info_t){PACK_DIR_NONE, PACK_TYPE_NONE, 0, 0};
  }
}


#define IS_PADDING_CHAR_A(c) (char_class((unsigned char)(c)) & (CHAR_NULL | CHAR_SPACE))
#define IS_PADDING_CHAR_Z(c) (char_class((unsigned char)(c)) & CHAR_NULL)

static int
hex2int(unsigned char ch)
{
  return hex_lookup[ch];
}

static mrb_value
str_len_ensure(mrb_state *mrb, mrb_value str, mrb_int len)
{
  mrb_int n = RSTRING_LEN(str);
  if (len < 0) {
    mrb_raise(mrb, E_RANGE_ERROR, "negative (or overflowed) integer");
  }
  if (len > n) {
    do {
      n *= 2;
    } while (len > n);
    str = mrb_str_resize(mrb, str, n);
  }
  return str;
}


static int
pack_char(mrb_state *mrb, mrb_value o, mrb_value str, mrb_int sidx, unsigned int flags)
{
  str = str_len_ensure(mrb, str, sidx + 1);
  RSTRING_PTR(str)[sidx] = (char)mrb_integer(o);
  return 1;
}

static int
unpack_char(mrb_state *mrb, const void *src, mrb_int srclen, mrb_value ary, unsigned int flags)
{
  if (flags & PACK_FLAG_SIGNED)
    mrb_ary_push(mrb, ary, mrb_fixnum_value(*(signed char*)src));
  else
    mrb_ary_push(mrb, ary, mrb_fixnum_value(*(unsigned char*)src));
  return 1;
}

static int
pack_short(mrb_state *mrb, mrb_value o, mrb_value str, mrb_int sidx, unsigned int flags)
{
  str = str_len_ensure(mrb, str, sidx + 2);
  uint16_t n = (uint16_t)mrb_integer(o);
  char *dptr = RSTRING_PTR(str) + sidx;

  /* use lookup tables to eliminate branching */
  const int *idx = (flags & PACK_FLAG_LITTLEENDIAN) ? le_idx16 : be_idx16;
  dptr[idx[0]] = (char)(n & 0xff);
  dptr[idx[1]] = (char)(n >> 8);

  return 2;
}

static int
unpack_short(mrb_state *mrb, const unsigned char *src, mrb_int srclen, mrb_value ary, unsigned int flags)
{
  /* use lookup tables to eliminate branching */
  const int *idx = (flags & PACK_FLAG_LITTLEENDIAN) ? le_idx16 : be_idx16;
  int n = (src[idx[1]] << 8) | src[idx[0]];

  if ((flags & PACK_FLAG_SIGNED) && (n >= 0x8000)) {
    n -= 0x10000;
  }
  mrb_ary_push(mrb, ary, mrb_fixnum_value(n));
  return 2;
}

static int
pack_long(mrb_state *mrb, mrb_value o, mrb_value str, mrb_int sidx, unsigned int flags)
{
  str = str_len_ensure(mrb, str, sidx + 4);
  uint32_t n = (uint32_t)mrb_integer(o);
  char *dptr = RSTRING_PTR(str) + sidx;

  /* use lookup tables to eliminate branching */
  const int *idx = (flags & PACK_FLAG_LITTLEENDIAN) ? le_idx32 : be_idx32;
  dptr[idx[0]] = (char)(n & 0xff);
  dptr[idx[1]] = (char)(n >> 8);
  dptr[idx[2]] = (char)(n >> 16);
  dptr[idx[3]] = (char)(n >> 24);

  return 4;
}

#ifndef MRB_INT64
static void
u32tostr(char *buf, size_t len, uint32_t n)
{
#ifdef MRB_NO_STDIO
  char *bufend = buf + len;
  char *p = bufend - 1;

  if (len < 1) {
    return;
  }

  *p-- = '\0';
  len--;

  if (n > 0) {
    for (; len > 0 && n > 0; len --, n /= 10) {
      *p -- = '0' + (n % 10);
    }
    p++;
  }
  else if (len > 0) {
    *p = '0';
    len--;
  }

  memmove(buf, p, bufend - p);
#else
  snprintf(buf, len, "%" PRIu32, n);
#endif /* MRB_NO_STDIO */
}
#endif /* MRB_INT64 */

static int
unpack_long(mrb_state *mrb, const unsigned char *src, mrb_int srclen, mrb_value ary, unsigned int flags)
{
#ifndef MRB_INT64
  char msg[60];
#endif
  uint32_t ul;
  mrb_int n;

  /* use lookup tables to eliminate branching */
  const int *idx = (flags & PACK_FLAG_LITTLEENDIAN) ? le_idx32 : be_idx32;
  ul = ((uint32_t)src[idx[3]] << 24) |
       ((uint32_t)src[idx[2]] << 16) |
       ((uint32_t)src[idx[1]] << 8) |
       (uint32_t)src[idx[0]];

  if (flags & PACK_FLAG_SIGNED) {
    n = (int32_t)ul;
  }
  else {
#ifndef MRB_INT64
    if (UINT_OVERFLOW_P(ul)) {
      u32tostr(msg, sizeof(msg), ul);
      mrb_raisef(mrb, E_RANGE_ERROR, "cannot unpack to Integer: %s", msg);
    }
#endif
    n = ul;
  }
  mrb_ary_push(mrb, ary, mrb_int_value(mrb, n));
  return 4;
}

static int
pack_quad(mrb_state *mrb, mrb_value o, mrb_value str, mrb_int sidx, unsigned int flags)
{
  str = str_len_ensure(mrb, str, sidx + 8);
  uint64_t n = (uint64_t)mrb_integer(o);
  char *dptr = RSTRING_PTR(str) + sidx;

  /* use lookup tables to eliminate branching */
  const int *idx = (flags & PACK_FLAG_LITTLEENDIAN) ? le_idx64 : be_idx64;
  dptr[idx[0]] = (char)(n & 0xff);
  dptr[idx[1]] = (char)(n >> 8);
  dptr[idx[2]] = (char)(n >> 16);
  dptr[idx[3]] = (char)(n >> 24);
  dptr[idx[4]] = (char)(n >> 32);
  dptr[idx[5]] = (char)(n >> 40);
  dptr[idx[6]] = (char)(n >> 48);
  dptr[idx[7]] = (char)(n >> 56);

  return 8;
}

static void
u64tostr(char *buf, size_t len, uint64_t n)
{
#ifdef MRB_NO_STDIO
  mrb_assert(len > 0);

  if (n < 10) {
    buf[0] = '0' + n;
    buf[1] = '\0';
    return;
  }

  char *bufend = buf + len;
  char *p = bufend - 1;

  *p-- = '\0';
  len--;

  for (; len > 0 && n > 0; len--, n /= 10) {
    *p-- = '0' + (n % 10);
  }
  p++;
  memmove(buf, p, bufend - p);
#else
  snprintf(buf, len, "%" PRIu64, n);
#endif /* MRB_NO_STDIO */
}

#ifndef MRB_INT64
static void
i64tostr(char *buf, size_t len, int64_t n)
{
#ifdef MRB_NO_STDIO
  mrb_assert(len > 0);

  if (n < 0) {
    *buf++ = '-';
    len--;
    n = -n;
  }

  u64tostr(buf, len, (uint64_t)n);
#else
  snprintf(buf, len, "%" PRId64, n);
#endif /* MRB_NO_STDIO */
}
#endif /* MRB_INT64 */

static int
unpack_quad(mrb_state *mrb, const unsigned char *src, mrb_int srclen, mrb_value ary, unsigned int flags)
{
  char msg[60];
  mrb_int n;

  /* use lookup tables to eliminate branching */
  const int *idx = (flags & PACK_FLAG_LITTLEENDIAN) ? le_idx64 : be_idx64;
  uint64_t ull = ((uint64_t)src[idx[7]] << 56) |
                 ((uint64_t)src[idx[6]] << 48) |
                 ((uint64_t)src[idx[5]] << 40) |
                 ((uint64_t)src[idx[4]] << 32) |
                 ((uint64_t)src[idx[3]] << 24) |
                 ((uint64_t)src[idx[2]] << 16) |
                 ((uint64_t)src[idx[1]] << 8) |
                 (uint64_t)src[idx[0]];

  if (flags & PACK_FLAG_SIGNED) {
    int64_t sll = ull;
#ifndef MRB_INT64
    if (INT_OVERFLOW_P(sll)) {
      i64tostr(msg, sizeof(msg), sll);
      mrb_raisef(mrb, E_RANGE_ERROR, "cannot unpack to Integer: %s", msg);
    }
#endif
    n = (mrb_int)sll;
  }
  else {
    if (UINT_OVERFLOW_P(ull)) {
      u64tostr(msg, sizeof(msg), ull);
      mrb_raisef(mrb, E_RANGE_ERROR, "cannot unpack to Integer: %s", msg);
    }
    n = (mrb_int)ull;
  }
  mrb_ary_push(mrb, ary, mrb_int_value(mrb, n));
  return 8;
}

static int
pack_BER(mrb_state *mrb, mrb_value o, mrb_value str, mrb_int sidx, unsigned int flags)
{
  mrb_int n = mrb_integer(o);

  if (n < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "can't compress negative numbers");
  }

  /* fast path for 1-byte values (0-127) */
  if (n < 128) {
    str = str_len_ensure(mrb, str, sidx + 1);
    RSTRING_PTR(str)[sidx] = (char)n;
    return 1;
  }

  /* fast path for 2-byte values (128-16383) */
  if (n < 16384) {
    str = str_len_ensure(mrb, str, sidx + 2);
    char *p = RSTRING_PTR(str) + sidx;
    *p++ = (char)((n >> 7) | 0x80);
    *p = (char)(n & 0x7f);
    return 2;
  }

  /* original algorithm for larger values */
  int i;
  for (i = 1; i < (int)sizeof(mrb_int) + 1; i++) {
    mrb_int mask = ~((1L << (7 * i)) - 1);
    if ((n & mask) == 0) break;
  }

  str = str_len_ensure(mrb, str, sidx + i);
  char *p = RSTRING_PTR(str) + sidx;

  for (size_t j = i; j > 0; p++, j--) {
    mrb_int x = (n >> (7 * (j - 1))) & 0x7f;
    *p = (char)x;
    if (j > 1) *p |= 0x80;
  }

  return i;
}

static int
unpack_BER(mrb_state *mrb, const unsigned char *src, mrb_int srclen, mrb_value ary, unsigned int flags)
{
  mrb_int n = 0;
  const unsigned char *p = src;
  const unsigned char *e = p + srclen;

  if (srclen == 0) return 0;

  /* calculate maximum safe bytes before potential overflow */
  const int max_safe_bytes = (sizeof(mrb_int) * 8 - 1) / 7;  /* conservative estimate */

  int i;
  for (i = 1; p < e; p++, i++) {
    /* check overflow before we might exceed safe limits */
    if (i > max_safe_bytes || n > (MRB_INT_MAX >> 7)) {
      mrb_raise(mrb, E_RANGE_ERROR, "BER unpacking 'w' overflow");
    }

    n <<= 7;
    n |= *p & 0x7f;
    if ((*p & 0x80) == 0) break;
  }

  mrb_ary_push(mrb, ary, mrb_int_value(mrb, n));
  return i;
}

#ifndef MRB_NO_FLOAT
static int
pack_double(mrb_state *mrb, mrb_value o, mrb_value str, mrb_int sidx, unsigned int flags)
{
  union {
    double d;
    uint8_t bytes[8];
  } converter;

  str = str_len_ensure(mrb, str, sidx + 8);
  converter.d = mrb_float(o);
  char *dptr = RSTRING_PTR(str) + sidx;

  /* use lookup tables to eliminate branching */
  const int *idx = (flags & PACK_FLAG_LITTLEENDIAN) ? le_idx64 : be_idx64;
  dptr[idx[0]] = converter.bytes[0];
  dptr[idx[1]] = converter.bytes[1];
  dptr[idx[2]] = converter.bytes[2];
  dptr[idx[3]] = converter.bytes[3];
  dptr[idx[4]] = converter.bytes[4];
  dptr[idx[5]] = converter.bytes[5];
  dptr[idx[6]] = converter.bytes[6];
  dptr[idx[7]] = converter.bytes[7];

  return 8;
}

static int
unpack_double(mrb_state *mrb, const unsigned char * src, mrb_int srclen, mrb_value ary, unsigned int flags)
{
  union {
    double d;
    uint8_t bytes[8];
  } converter;

  /* use lookup tables to eliminate branching */
  const int *idx = (flags & PACK_FLAG_LITTLEENDIAN) ? le_idx64 : be_idx64;
  converter.bytes[0] = src[idx[0]];
  converter.bytes[1] = src[idx[1]];
  converter.bytes[2] = src[idx[2]];
  converter.bytes[3] = src[idx[3]];
  converter.bytes[4] = src[idx[4]];
  converter.bytes[5] = src[idx[5]];
  converter.bytes[6] = src[idx[6]];
  converter.bytes[7] = src[idx[7]];

  mrb_ary_push(mrb, ary, mrb_float_value(mrb, converter.d));

  return 8;
}

static int
pack_float(mrb_state *mrb, mrb_value o, mrb_value str, mrb_int sidx, unsigned int flags)
{
  union {
    float f;
    uint8_t bytes[4];
  } converter;

  str = str_len_ensure(mrb, str, sidx + 4);
  converter.f = (float)mrb_float(o);
  char *dptr = RSTRING_PTR(str) + sidx;

  /* use lookup tables to eliminate branching */
  const int *idx = (flags & PACK_FLAG_LITTLEENDIAN) ? le_idx32 : be_idx32;
  dptr[idx[0]] = converter.bytes[0];
  dptr[idx[1]] = converter.bytes[1];
  dptr[idx[2]] = converter.bytes[2];
  dptr[idx[3]] = converter.bytes[3];

  return 4;
}

static int
unpack_float(mrb_state *mrb, const unsigned char * src, mrb_int srclen, mrb_value ary, unsigned int flags)
{
  union {
    float f;
    uint8_t bytes[4];
  } converter;

  /* use lookup tables to eliminate branching */
  const int *idx = (flags & PACK_FLAG_LITTLEENDIAN) ? le_idx32 : be_idx32;
  converter.bytes[0] = src[idx[0]];
  converter.bytes[1] = src[idx[1]];
  converter.bytes[2] = src[idx[2]];
  converter.bytes[3] = src[idx[3]];

  mrb_ary_push(mrb, ary, mrb_float_value(mrb, converter.f));

  return 4;
}
#endif

static int
pack_utf8(mrb_state *mrb, mrb_value o, mrb_value str, mrb_int sidx, int count, unsigned int flags)
{
  char utf8[4];
  int len = 0;
  uint32_t c = 0;

  c = (uint32_t)mrb_integer(o);

  /* Unicode character */
  /* from mruby-compiler gem */
  if (c < 0x80) {
    utf8[0] = (char)c;
    len = 1;
  }
  else if (c < 0x800) {
    utf8[0] = (char)(0xC0 | (c >> 6));
    utf8[1] = (char)(0x80 | (c & 0x3F));
    len = 2;
  }
  else if (c < 0x10000) {
    utf8[0] = (char)(0xE0 |  (c >> 12)        );
    utf8[1] = (char)(0x80 | ((c >>  6) & 0x3F));
    utf8[2] = (char)(0x80 | ( c        & 0x3F));
    len = 3;
  }
  else if (c < 0x200000) {
    utf8[0] = (char)(0xF0 |  (c >> 18)        );
    utf8[1] = (char)(0x80 | ((c >> 12) & 0x3F));
    utf8[2] = (char)(0x80 | ((c >>  6) & 0x3F));
    utf8[3] = (char)(0x80 | ( c        & 0x3F));
    len = 4;
  }
  else {
    mrb_raise(mrb, E_RANGE_ERROR, "pack(U): value out of range");
  }

  str = str_len_ensure(mrb, str, sidx + len);
  memcpy(RSTRING_PTR(str) + sidx, utf8, len);

  return len;
}



static int
unpack_utf8(mrb_state *mrb, const unsigned char * src, mrb_int srclen, mrb_value ary, unsigned int flags)
{
  if (srclen == 0) {
    return 1;
  }

  const unsigned char *p = src;
  uint8_t first_byte = *p;

  /* ASCII fast path - most common case */
  if (first_byte < 0x80) {
    mrb_ary_push(mrb, ary, mrb_fixnum_value(first_byte));
    return 1;
  }

  /* Multi-byte UTF-8 with optimized lookup table */
  uint8_t seq_len = utf8_seq_len_high[first_byte - 0x80];
  if (seq_len == 0 || seq_len > srclen) {
    goto malformed;
  }

  /* Inline 2-byte sequence optimization - common case */
  if (seq_len == 2) {
    if (!IS_UTF8_CONTINUATION(p[1])) {
      goto malformed;
    }
    uint32_t uv = ((first_byte & 0x1F) << 6) | (p[1] & 0x3F);
    /* validate minimum value for 2-byte sequence */
    if (uv >= 0x80) {
      mrb_ary_push(mrb, ary, mrb_fixnum_value((mrb_int)uv));
      return 2;
    }
    goto redundant;
  }

  /* Inline 3-byte sequence optimization */
  if (seq_len == 3) {
    if (!IS_UTF8_CONTINUATION(p[1]) || !IS_UTF8_CONTINUATION(p[2])) {
      goto malformed;
    }
    uint32_t uv = ((first_byte & 0x0F) << 12) |
                  ((p[1] & 0x3F) << 6) |
                  (p[2] & 0x3F);
    /* validate minimum value for 3-byte sequence */
    if (uv >= 0x800) {
      mrb_ary_push(mrb, ary, mrb_fixnum_value((mrb_int)uv));
      return 3;
    }
    goto redundant;
  }

  /* 4-byte sequence - less common, use original implementation */
  if (seq_len == 4) {
    if (!IS_UTF8_CONTINUATION(p[1]) || !IS_UTF8_CONTINUATION(p[2]) || !IS_UTF8_CONTINUATION(p[3])) {
      goto malformed;
    }
    uint32_t uv = ((first_byte & 0x07) << 18) |
                  ((p[1] & 0x3F) << 12) |
                  ((p[2] & 0x3F) << 6) |
                  (p[3] & 0x3F);
    /* validate minimum value and maximum valid Unicode */
    if (uv >= 0x10000 && uv <= 0x10FFFF) {
      mrb_ary_push(mrb, ary, mrb_fixnum_value((mrb_int)uv));
      return 4;
    }
    if (uv < 0x10000) goto redundant;
    goto malformed;
  }

malformed:
  mrb_raise(mrb, E_ARGUMENT_ERROR, "malformed UTF-8 character");

redundant:
  mrb_raise(mrb, E_ARGUMENT_ERROR, "redundant UTF-8 sequence");
}

static int
pack_str(mrb_state *mrb, mrb_value src, mrb_value dst, mrb_int didx, int count, unsigned int flags)
{
  const char *sptr = RSTRING_PTR(src);
  mrb_int slen = RSTRING_LEN(src);
  mrb_int copylen, padlen;
  char pad;

  /* determine padding character based on format */
  if ((flags & PACK_FLAG_a) || (flags & PACK_FLAG_Z))
    pad = '\0';
  else
    pad = ' ';

  if (count == 0) {
    return 0;
  }
  else if (count == -1) {
    copylen = slen;
    padlen = (flags & PACK_FLAG_Z) ? 1 : 0;
  }
  else if (count < slen) {
    copylen = count;
    padlen = 0;
  }
  else {
    copylen = slen;
    padlen = count - slen;
  }

  /* pre-allocate exact buffer size */
  dst = str_len_ensure(mrb, dst, didx + copylen + padlen);
  char *dptr = RSTRING_PTR(dst) + didx;
  char *dptr0 = dptr;

  /* copy string data */
  if (copylen > 0) {
    memcpy(dptr, sptr, copylen);
    dptr += copylen;
  }

  /* bulk padding using memset instead of loop */
  if (padlen > 0) {
    memset(dptr, pad, padlen);
    dptr += padlen;
  }

  return (int)(dptr - dptr0);
}

#define CHECK_UNPACK_LEN(mrb, slen, ary) do {\
  if ((slen) <= 0) {\
    mrb_ary_push(mrb, ary, mrb_str_new(mrb, 0, 0));\
    return 0;\
  }\
} while (0)

static int
unpack_str(mrb_state *mrb, const void *src, mrb_int slen, mrb_value ary, int count, unsigned int flags)
{
  CHECK_UNPACK_LEN(mrb, slen, ary);

  const char *sptr = (const char*)src;
  int copylen;

  if (count != -1 && count < slen) {
    slen = count;
  }
  copylen = (int)slen;

  if (slen >= 0 && flags & PACK_FLAG_Z) {  /* "Z" format */
    const char *cp = (const char*)memchr(sptr, '\0', slen);
    if (cp != NULL) {
      copylen = (int)(cp - sptr);
      if (count == -1) {
        slen = copylen + 1;
      }
    }
  }
  else if (!(flags & PACK_FLAG_a)) {  /* "A" format - trim spaces and nulls */
    /* optimized reverse trimming using lookup table */
    while (copylen > 0 && IS_PADDING_CHAR_A(sptr[copylen - 1])) {
      copylen--;
    }
  }
  /* "a" format does no trimming */

  if (copylen < 0) copylen = 0;

  mrb_value dst = mrb_str_new(mrb, sptr, (mrb_int)copylen);
  mrb_ary_push(mrb, ary, dst);
  return (int)slen;
}


static int
pack_hex(mrb_state *mrb, mrb_value src, mrb_value dst, mrb_int didx, int count, unsigned int flags)
{
  char *sptr = RSTRING_PTR(src);
  long slen = (long)RSTRING_LEN(src);

  unsigned int ashift, bshift;
  if (flags & PACK_FLAG_LSB) {
    ashift = 0;
    bshift = 4;
  }
  else {
    ashift = 4;
    bshift = 0;
  }

  if (count == -1) {
    count = slen;
  }
  else if (slen > count) {
    slen = count;
  }

  /* calculate output buffer size needed - one byte per two hex chars */
  /* use count/2 + (count&1) to avoid overflow when count == INT_MAX */
  int output_bytes = count / 2 + (count & 1);
  dst = str_len_ensure(mrb, dst, didx + output_bytes);
  char *dptr = RSTRING_PTR(dst) + didx;
  char *dptr0 = dptr;

  /* process pairs of hex characters */
  while (slen >= 2) {
    int a = hex2int((unsigned char)*sptr++);
    if (a < 0) break;
    int b = hex2int((unsigned char)*sptr++);
    if (b < 0) break;

    *dptr++ = (a << ashift) + (b << bshift);
    slen -= 2;
  }

  /* handle odd remaining character */
  if (slen > 0) {
    int a = hex2int((unsigned char)*sptr);
    if (a >= 0) {
      *dptr++ = (a << ashift);
    }
  }

  return (int)(dptr - dptr0);
}

static int
unpack_hex(mrb_state *mrb, const void *src, mrb_int slen, mrb_value ary, int count, unsigned int flags)
{
  CHECK_UNPACK_LEN(mrb, slen, ary);

  int ashift, bshift;
  if (flags & PACK_FLAG_LSB) {
    ashift = 0;
    bshift = 4;
  }
  else {
    ashift = 4;
    bshift = 0;
  }

  const char *sptr = (const char*)src;
  const char *sptr0 = sptr;

  if (count == -1)
    count = (int)(slen * 2);

  mrb_value dst = mrb_str_new(mrb, NULL, count);
  char *dptr = RSTRING_PTR(dst);
  char *dptr0 = dptr;

  const char hexadecimal[] = "0123456789abcdef";

  /* process full bytes when we need pairs of hex characters */
  while (slen > 0 && count >= 2) {
    unsigned char byte = *sptr++;
    slen--;

    *dptr++ = hexadecimal[(byte >> ashift) & 0x0f];
    *dptr++ = hexadecimal[(byte >> bshift) & 0x0f];
    count -= 2;
  }

  /* handle remaining single character if count is odd */
  if (slen > 0 && count > 0) {
    unsigned char byte = *sptr++;
    *dptr++ = hexadecimal[(byte >> ashift) & 0x0f];
  }

  dst = mrb_str_resize(mrb, dst, (mrb_int)(dptr - dptr0));
  mrb_ary_push(mrb, ary, dst);
  return (int)(sptr - sptr0);
}

static int
pack_bstr(mrb_state *mrb, mrb_value src, mrb_value dst, mrb_int didx, int count, unsigned int flags)
{
  const char *sptr = RSTRING_PTR(src);
  int slen = (int)RSTRING_LEN(src);

  if (count == -1) {
    count = slen;
  }
  else if (slen > count) {
    slen = count;
  }

  /* calculate exact output size: (slen + 7) / 8 */
  int output_bytes = (slen + 7) / 8;
  dst = str_len_ensure(mrb, dst, didx + output_bytes);
  char *dptr = RSTRING_PTR(dst) + didx;
  char *dptr0 = dptr;

  /* select batch processing function based on bit order */
  uint8_t (*pack_func)(const char *) = (flags & PACK_FLAG_LSB) ? pack_8_bits_lsb : pack_8_bits_msb;

  /* process 8 characters at a time */
  int full_bytes = slen / 8;
  for (int i = 0; i < full_bytes; i++) {
    *dptr++ = (char)pack_func(sptr);
    sptr += 8;
  }

  /* handle remaining bits (partial byte) */
  int remaining_bits = slen % 8;
  if (remaining_bits > 0) {
    char temp_chars[8] = {'0', '0', '0', '0', '0', '0', '0', '0'};
    /* copy remaining characters, padding with '0' */
    for (int i = 0; i < remaining_bits; i++) {
      temp_chars[i] = sptr[i];
    }
    *dptr++ = (char)pack_func(temp_chars);
  }

  return (int)(dptr - dptr0);
}

static int
unpack_bstr(mrb_state *mrb, const void *src, mrb_int slen, mrb_value ary, int count, unsigned int flags)
{
  CHECK_UNPACK_LEN(mrb, slen, ary);

  const char *sptr0 = (const char*)src;
  const char *sptr = sptr0;

  if (count == -1 || count > slen * 8)
    count = (int)(slen * 8);

  /* pre-allocate exact output size */
  mrb_value dst = mrb_str_new(mrb, NULL, count);
  char *dptr = RSTRING_PTR(dst);
  char *dptr0 = dptr;

  /* select batch processing function based on bit order */
  void (*unpack_func)(uint8_t, char *) = (flags & PACK_FLAG_LSB) ? unpack_8_bits_lsb : unpack_8_bits_msb;

  /* process 8 bits (1 byte) at a time */
  int full_bytes = count / 8;
  for (int i = 0; i < full_bytes; i++) {
    unpack_func((uint8_t)*sptr++, dptr);
    dptr += 8;
  }

  /* handle remaining bits (partial byte) */
  int remaining_bits = count % 8;
  if (remaining_bits > 0) {
    char temp_chars[8];
    unpack_func((uint8_t)*sptr++, temp_chars);
    /* copy only the required number of characters */
    for (int i = 0; i < remaining_bits; i++) {
      *dptr++ = temp_chars[i];
    }
  }

  /* ensure string is properly sized */
  dst = mrb_str_resize(mrb, dst, (mrb_int)(dptr - dptr0));
  mrb_ary_push(mrb, ary, dst);
  return (int)(sptr - sptr0);
}

static int
pack_base64(mrb_state *mrb, mrb_value src, mrb_value dst, mrb_int didx, int count)
{
  char *srcptr = RSTRING_PTR(src);
  mrb_int srclen = RSTRING_LEN(src);

  if (srclen == 0) {
    return 0;
  }

  if (count != 0 && count < 3) {
    count = 45;
  }
  else if (count >= 3) {
    count -= count % 3;
  }

  /* precise memory allocation */
  mrb_int dstlen = (srclen + 2) / 3 * 4;
  if (count > 0) {
    dstlen += (srclen / count) + ((srclen % count) == 0 ? 0 : 1);
  }
  dst = str_len_ensure(mrb, dst, didx + dstlen);
  char *dstptr = RSTRING_PTR(dst) + didx;
  char *dstptr0 = dstptr;

  if (count == 0) {
    /* fast path: no line wrapping */
    while (srclen >= 3) {
      unsigned long l = (unsigned char)*srcptr++ << 16;
      l += (unsigned char)*srcptr++ << 8;
      l += (unsigned char)*srcptr++;
      srclen -= 3;

      *dstptr++ = base64chars[(l >> 18) & 0x3f];
      *dstptr++ = base64chars[(l >> 12) & 0x3f];
      *dstptr++ = base64chars[(l >> 6) & 0x3f];
      *dstptr++ = base64chars[l & 0x3f];
    }
  }
  else {
    /* line wrapping path */
    mrb_int column = 3;
    while (srclen >= 3) {
      unsigned long l = (unsigned char)*srcptr++ << 16;
      l += (unsigned char)*srcptr++ << 8;
      l += (unsigned char)*srcptr++;
      srclen -= 3;

      *dstptr++ = base64chars[(l >> 18) & 0x3f];
      *dstptr++ = base64chars[(l >> 12) & 0x3f];
      *dstptr++ = base64chars[(l >> 6) & 0x3f];
      *dstptr++ = base64chars[l & 0x3f];

      if (column == count) {
        *dstptr++ = '\n';
        column = 0;
      }
      column += 3;
    }

    /* handle remaining 1-2 bytes */
    if (srclen == 1) {
      unsigned long l = (unsigned char)*srcptr << 16;
      *dstptr++ = base64chars[(l >> 18) & 0x3f];
      *dstptr++ = base64chars[(l >> 12) & 0x3f];
      *dstptr++ = '=';
      *dstptr++ = '=';
      column += 3;
    }
    else if (srclen == 2) {
      unsigned long l = (unsigned char)*srcptr++ << 16;
      l += (unsigned char)*srcptr << 8;
      *dstptr++ = base64chars[(l >> 18) & 0x3f];
      *dstptr++ = base64chars[(l >> 12) & 0x3f];
      *dstptr++ = base64chars[(l >> 6) & 0x3f];
      *dstptr++ = '=';
      column += 3;
    }

    if (column > 0) {
      *dstptr++ = '\n';
    }
    return (int)(dstptr - dstptr0);
  }

  /* handle remaining 1-2 bytes for fast path */
  if (srclen == 1) {
    unsigned long l = (unsigned char)*srcptr << 16;
    *dstptr++ = base64chars[(l >> 18) & 0x3f];
    *dstptr++ = base64chars[(l >> 12) & 0x3f];
    *dstptr++ = '=';
    *dstptr++ = '=';
  }
  else if (srclen == 2) {
    unsigned long l = (unsigned char)*srcptr++ << 16;
    l += (unsigned char)*srcptr << 8;
    *dstptr++ = base64chars[(l >> 18) & 0x3f];
    *dstptr++ = base64chars[(l >> 12) & 0x3f];
    *dstptr++ = base64chars[(l >> 6) & 0x3f];
    *dstptr++ = '=';
  }

  return (int)(dstptr - dstptr0);
}

static int
unpack_base64(mrb_state *mrb, const void *src, mrb_int slen, mrb_value ary)
{
  CHECK_UNPACK_LEN(mrb, slen, ary);

  const char *sptr0 = (const char*)src;
  const char *sptr = sptr0;

  /* estimate buffer size - may be shorter due to padding/whitespace */
  int dlen = (int)(slen / 4 * 3);
  mrb_value dst = mrb_str_new(mrb, NULL, dlen);
  char *dptr0 = RSTRING_PTR(dst);
  char *dptr = dptr0;

  int padding = 0;
  while (slen >= 4) {
    unsigned char ch[4];

    /* collect 4 valid base64 characters */
    for (int i = 0; i < 4; i++) {
      unsigned char c;
      do {
        if (slen-- == 0)
          goto done;
        c = *sptr++;
        if (c >= sizeof(base64_dec_tab))
          continue;
        ch[i] = base64_dec_tab[c];
        if (ch[i] == PACK_BASE64_PADDING) {
          ch[i] = 0;
          padding++;
        }
      } while (c >= sizeof(base64_dec_tab) || ch[i] == PACK_BASE64_IGNORE);
    }

    /* decode 4 characters to 3 bytes */
    unsigned long l = (ch[0] << 18) + (ch[1] << 12) + (ch[2] << 6) + ch[3];

    if (padding == 0) {
      *dptr++ = (l >> 16) & 0xff;
      *dptr++ = (l >> 8) & 0xff;
      *dptr++ = l & 0xff;
    }
    else if (padding == 1) {
      *dptr++ = (l >> 16) & 0xff;
      *dptr++ = (l >> 8) & 0xff;
      break;
    }
    else {
      *dptr++ = (l >> 16) & 0xff;
      break;
    }
  }

done:
  dst = mrb_str_resize(mrb, dst, (mrb_int)(dptr - dptr0));
  mrb_ary_push(mrb, ary, dst);
  return (int)(sptr - sptr0);
}

static int
pack_qenc(mrb_state *mrb, mrb_value src, mrb_value dst, mrb_int didx, int count)
{
  static const char hex_table[] = "0123456789ABCDEF";
  char buff[1024];
  char *s = RSTRING_PTR(src);
  char *send = s + RSTRING_LEN(src);
  int i = 0, n = 0, prev = EOF;
  int dlen = 0;

  if (count <= 1) count = 72;

  while (s < send) {
    unsigned char byte = (unsigned char)*s;
    uint8_t encode_type = qprint_encode_type[byte];

    /* ASCII printable fast path - most common case */
    if (encode_type == 0) {
      /* Handle space/tab at line end special case */
      if ((byte == ' ' || byte == '\t') && s + 1 < send && *(s + 1) == '\n') {
        /* Space/tab before newline needs encoding */
        buff[i++] = '=';
        buff[i++] = hex_table[(byte & 0xf0) >> 4];
        buff[i++] = hex_table[byte & 0x0f];
        n += 3;
        prev = EOF;
      }
      else {
        /* Regular printable character - direct copy */
        buff[i++] = byte;
        n++;
        prev = byte;
      }
    }
    /* Newline special handling */
    else if (encode_type == 2) {
      if (prev == ' ' || prev == '\t') {
        buff[i++] = '=';
        buff[i++] = byte;
      }
      buff[i++] = byte;
      n = 0;
      prev = byte;
    }
    /* Character needs encoding */
    else {
      buff[i++] = '=';
      buff[i++] = hex_table[(byte & 0xf0) >> 4];
      buff[i++] = hex_table[byte & 0x0f];
      n += 3;
      prev = EOF;
    }

    /* Check line length limit */
    if (n > count) {
      buff[i++] = '=';
      buff[i++] = '\n';
      n = 0;
      prev = '\n';
    }

    /* Flush buffer if getting full */
    if (i > 1024 - 5) {
      str_len_ensure(mrb, dst, didx+dlen+i);
      memcpy(RSTRING_PTR(dst)+didx+dlen, buff, i);
      dlen += i;
      i = 0;
    }
    s++;
  }

  /* Add final soft line break if needed */
  if (n > 0) {
    buff[i++] = '=';
    buff[i++] = '\n';
  }

  /* Flush remaining buffer */
  if (i > 0) {
    str_len_ensure(mrb, dst, didx+dlen+i);
    memcpy(RSTRING_PTR(dst)+didx+dlen, buff, i);
    dlen += i;
  }

  return dlen;
}

static int
unpack_qenc(mrb_state *mrb, const void *src, mrb_int slen, mrb_value ary)
{
  CHECK_UNPACK_LEN(mrb, slen, ary);

  mrb_value buf = mrb_str_new(mrb, 0, slen);
  const char *s = (const char*)src;
  const char *send = s + slen;
  char *ptr = RSTRING_PTR(buf);
  int c1, c2;

  while (s < send) {
    /* Fast path for non-encoded characters - most common case */
    if (*s != '=') {
      *ptr++ = *s;
      s++;
      continue;
    }

    /* Handle =XX encoded sequences */
    if (++s == send) break;

    /* Handle soft line breaks: =\r\n or =\n */
    if (s + 1 < send && *s == '\r' && *(s + 1) == '\n') {
      s += 2;  /* Skip \r\n */
      continue;
    }
    if (*s == '\n') {
      s++;     /* Skip \n */
      continue;
    }

    /* Decode =XX hex sequence */
    if ((c1 = hex2int(*s)) == -1) break;
    if (++s == send) break;
    if ((c2 = hex2int(*s)) == -1) break;
    *ptr++ = (char)(c1 << 4 | c2);
    s++;
  }

  buf = mrb_str_resize(mrb, buf, (mrb_int)(ptr - RSTRING_PTR(buf)));
  mrb_ary_push(mrb, ary, buf);
  return (int)slen;
}

static int
pack_uu(mrb_state *mrb, mrb_value src, mrb_value dst, mrb_int didx, int count)
{
  char *s = RSTRING_PTR(src);
  int slen = (int)RSTRING_LEN(src);
  int lines_written = 0;
  int dlen = 0;

  if (count <= 1) count = 45; /* default line length for UU-encoding */

  /* Calculate buffer size by accounting for per-line encoding
   * Each line encodes separately, so padding happens per line, not globally
   */
  mrb_int num_lines = (slen + count - 1) / count;  /* Number of lines */
  mrb_int total_encoded = 0;
  mrb_int temp_slen = slen;

  /* Calculate actual encoded size line by line */
  while (temp_slen > 0) {
    mrb_int line_len = (temp_slen > count) ? count : temp_slen;
    total_encoded += ((line_len + 2) / 3) * 4;  /* Each line's encoded size */
    temp_slen -= line_len;
  }

  /* Total buffer = encoded data + (length char + newline) per line + terminating line */
  mrb_int buffer_size = total_encoded + num_lines * 2 + 2;
  str_len_ensure(mrb, dst, didx + buffer_size);
  char *dptr = RSTRING_PTR(dst) + didx;

  while (slen > 0) {
    int line_len = (slen > count) ? count : slen;

    /* Write line length character */
    *dptr++ = uu_encode_table[line_len & 0x3F];
    dlen++;

    int processed = 0;
    while (processed < line_len) {
      /* Process groups of 3 bytes -> 4 characters */
      uint32_t group = 0;
      int bytes_in_group = 0;

      /* Read up to 3 bytes */
      for (int i = 0; i < 3 && processed < line_len; i++) {
        group = (group << 8) | (unsigned char)s[processed++];
        bytes_in_group++;
      }

      /* Pad incomplete group with zeros */
      group <<= (3 - bytes_in_group) * 8;

      /* Extract 4 groups of 6 bits and encode */
      *dptr++ = uu_encode_table[(group >> 18) & 0x3F];
      *dptr++ = uu_encode_table[(group >> 12) & 0x3F];
      *dptr++ = uu_encode_table[(group >> 6) & 0x3F];
      *dptr++ = uu_encode_table[group & 0x3F];
      dlen += 4;
    }

    /* Add newline */
    *dptr++ = '\n';
    dlen++;

    s += line_len;
    slen -= line_len;
    lines_written++;
  }

  /* Add terminating line if data was processed */
  if (lines_written > 0) {
    *dptr++ = uu_encode_table[0]; /* length 0 */
    *dptr++ = '\n';
    dlen += 2;
  }

  return dlen;
}

static int
unpack_uu(mrb_state *mrb, const void *src, mrb_int slen, mrb_value ary)
{
  const unsigned char *s = (const unsigned char*)src;
  const unsigned char *send = s + slen;
  mrb_value result = mrb_str_new(mrb, 0, slen * 3 / 4); /* estimate result size */
  char *dptr = RSTRING_PTR(result);
  char *dptr_start = dptr;

  while (s < send) {
    /* Skip empty lines and whitespace */
    while (s < send && (*s == '\n' || *s == '\r' || *s == ' ' || *s == '\t')) {
      s++;
    }
    if (s >= send) break;

    /* Read line length */
    int line_len = 0;
    if (*s >= 32 && *s < 128 && uu_decode_table[*s - 32] >= 0) {
      line_len = uu_decode_table[*s - 32];
      s++;
    }
    else {
      break; /* Invalid length character */
    }

    /* Empty line indicates end */
    if (line_len == 0) {
      break;
    }

    /* Decode line data */
    int bytes_decoded = 0;
    while (bytes_decoded < line_len && s + 3 < send) {
      /* Decode 4 characters to 3 bytes */
      int c[4];
      int valid = 1;

      for (int i = 0; i < 4; i++) {
        if (s >= send || *s < 32 || *s >= 128 || uu_decode_table[*s - 32] < 0) {
          valid = 0;
          break;
        }
        c[i] = uu_decode_table[*s++ - 32];
      }

      if (!valid) break;

      /* Combine 4 x 6-bit values into 3 x 8-bit values */
      uint32_t group = (c[0] << 18) | (c[1] << 12) | (c[2] << 6) | c[3];

      /* Extract up to 3 bytes, don't exceed line length */
      int bytes_to_extract = (line_len - bytes_decoded > 3) ? 3 : (line_len - bytes_decoded);

      for (int i = 0; i < bytes_to_extract; i++) {
        *dptr++ = (group >> (16 - i * 8)) & 0xFF;
        bytes_decoded++;
      }
    }

    /* Skip to end of line */
    while (s < send && *s != '\n' && *s != '\r') {
      s++;
    }
  }

  result = mrb_str_resize(mrb, result, (mrb_int)(dptr - dptr_start));
  mrb_ary_push(mrb, ary, result);
  return (int)slen;
}

static int
pack_nul(mrb_state *mrb, mrb_value dst, mrb_int didx, int count)
{
  long i;

  dst = str_len_ensure(mrb, dst, didx + count);
  for (i = 0; i < count; i++) {
    RSTRING_PTR(dst)[didx + i] = '\0';
  }
  return count;
}

static void
check_x(mrb_state *mrb, mrb_int a, mrb_int count, char c)
{
  if (a < count) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "%c outside of string", c);
  }
}

static void
prepare_tmpl(mrb_state *mrb, struct tmpl *tmpl)
{
  mrb_get_args(mrb, "S", &tmpl->str);
  tmpl->idx = 0;
}

static int
has_tmpl(const struct tmpl *tmpl)
{
  return (tmpl->idx < RSTRING_LEN(tmpl->str));
}

static enum pack_dir
read_tmpl(mrb_state *mrb, struct tmpl *tmpl, enum pack_type *typep, mrb_int *sizep, mrb_int *countp, unsigned int *flagsp)
{
  mrb_int t, tlen;
  int ch, size = 0;
  enum pack_dir dir;
  enum pack_type type;
  mrb_int count = 1;
  unsigned int flags = 0;
  const char *tptr;

  tptr = RSTRING_PTR(tmpl->str);
  tlen = RSTRING_LEN(tmpl->str);

 restart:
  if (tmpl->idx >= tlen) return PACK_DIR_NONE;
  t = tptr[tmpl->idx++];
 alias:

  /* Handle whitespace - skip and restart */
  if (ISSPACE((char)t)) {
    goto restart;
  }

  /* Special handling for runtime-dependent formats and special characters */
  switch (t) {
  case 'I':
    switch (sizeof(int)) {
      case 2: t = 'S'; goto alias;
      case 4: t = 'L'; goto alias;
      case 8: t = 'Q'; goto alias;
      default:
        mrb_raisef(mrb, E_RUNTIME_ERROR, "mruby-pack does not support sizeof(int) == %d", (int)sizeof(int));
    }
    break;
  case 'i':
    switch (sizeof(int)) {
      case 2: t = 's'; goto alias;
      case 4: t = 'l'; goto alias;
      case 8: t = 'q'; goto alias;
      default:
        mrb_raisef(mrb, E_RUNTIME_ERROR, "mruby-pack does not support sizeof(int) == %d", (int)sizeof(int));
    }
    break;
  case 'J':
    switch (sizeof(intptr_t)) {
      case 4: t = 'L'; goto alias;
      case 8: t = 'Q'; goto alias;
      default:
        mrb_raisef(mrb, E_RUNTIME_ERROR, "mruby-pack does not support sizeof(uintptr_t) == %d", (int)sizeof(uintptr_t));
    }
    break;
  case 'j':
    switch (sizeof(intptr_t)) {
      case 4: t = 'l'; goto alias;
      case 8: t = 'q'; goto alias;
      default:
        mrb_raisef(mrb, E_RUNTIME_ERROR, "mruby-pack does not support sizeof(intptr_t) == %d", (int)sizeof(intptr_t));
    }
    break;
  case '#':
    while (++tmpl->idx < tlen && tptr[tmpl->idx] != '\n')
      ;
    goto restart;
  case 'p': case 'P':
  case '%':
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "%c is not supported", (char)t);
    break;
  default:
    /* Use O(1) lookup table for standard format characters */
    if (t >= 0 && t < 256) {
      format_info_t info_val = get_format_info((unsigned char)t);
      const format_info_t *info = &info_val;
      if (info->dir != PACK_DIR_NONE) {
        /* Valid format character found in lookup table */
        dir = info->dir;
        type = info->type;
        size = info->size;
        flags = info->base_flags;
        break;
      }
    }

    /* Handle invalid characters */
    char c = (char)t;
    mrb_value s = mrb_str_new(mrb, &c, 1);
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "unknown unpack directive %!v", s);
  }

  /* read suffix [0-9*_!<>] */
  while (tmpl->idx < tlen) {
    ch = tptr[tmpl->idx];
    if (ISDIGIT(ch)) {
      char *e;
      mrb_int n;
      if (!mrb_read_int(tptr+tmpl->idx, tptr+tlen, &e, &n) || INT_MAX < n) {
        mrb_raise(mrb, E_RUNTIME_ERROR, "too big template length");
      }
      count = n;
      tmpl->idx = (int)(e - tptr);
      continue;
    }
    else if (ch == '*')  {
      if (type == PACK_TYPE_NONE)
        count = 0;
      else
        count = -1;
    }
    else if (ch == '_' || ch == '!' || ch == '<' || ch == '>') {
      if (strchr("sSiIlLqQ", (int)t) == NULL) {
        mrb_raisef(mrb, E_ARGUMENT_ERROR, "'%c' allowed only after types sSiIlLqQ", ch);
      }
      if (ch == '_' || ch == '!') {
        flags |= PACK_FLAG_s;
      }
      else if (ch == '<') {
        flags |= PACK_FLAG_LT;
      }
      else if (ch == '>') {
        flags |= PACK_FLAG_GT;
      }
    }
    else {
      break;
    }
    tmpl->idx++;
  }

  if ((flags & PACK_FLAG_LT) || (!(flags & PACK_FLAG_GT) && littleendian)) {
    flags |= PACK_FLAG_LITTLEENDIAN;
  }

  *typep = type;
  *sizep = size;
  *countp = count;
  *flagsp = flags;
  return dir;
}

/*
 * call-seq:
 *   array.pack(template) -> string
 *
 * Packs the contents of array into a binary string according to the
 * directives in template. Directives are single characters from the
 * table below. Each directive may be followed by a number indicating
 * the number of times to repeat the directive.
 *
 * Template string directives:
 *   C - 8-bit unsigned integer (unsigned char)
 *   c - 8-bit signed integer (signed char)
 *   S - 16-bit unsigned integer, native endian (uint16_t)
 *   s - 16-bit signed integer, native endian (int16_t)
 *   L - 32-bit unsigned integer, native endian (uint32_t)
 *   l - 32-bit signed integer, native endian (int32_t)
 *   Q - 64-bit unsigned integer, native endian (uint64_t)
 *   q - 64-bit signed integer, native endian (int64_t)
 *   n - 16-bit unsigned integer, network byte order
 *   N - 32-bit unsigned integer, network byte order
 *   v - 16-bit unsigned integer, little endian
 *   V - 32-bit unsigned integer, little endian
 *   f - single precision float, native format
 *   d - double precision float, native format
 *   A - ASCII string, space padded
 *   a - ASCII string, null padded
 *   Z - null-terminated string
 *   H - hex string, high nibble first
 *   h - hex string, low nibble first
 *   x - null byte
 *   X - back up one byte
 *   @ - null fill to absolute position
 *
 *   [1, 2, 3].pack("CCC")     #=> "\x01\x02\x03"
 *   [1, 2].pack("S*")         #=> "\x01\x00\x02\x00" (little endian)
 *   ["hello"].pack("A10")     #=> "hello     "
 */
static mrb_value
mrb_pack_pack(mrb_state *mrb, mrb_value ary)
{
  mrb_value o;
  struct tmpl tmpl;
  enum pack_type type;
  mrb_int count;
  mrb_int size;
  unsigned int flags;
  enum pack_dir dir;

  prepare_tmpl(mrb, &tmpl);

  mrb_value result = mrb_str_new(mrb, NULL, 128);  /* allocate initial buffer */
  mrb_int aidx = 0;
  mrb_int ridx = 0;
  while (has_tmpl(&tmpl)) {
    dir = read_tmpl(mrb, &tmpl, &type, &size, &count, &flags);

    if (dir == PACK_DIR_NONE) break;
    if (dir == PACK_DIR_NUL) {
    grow:
      if (ridx > INT_MAX - count) goto overflow;
      ridx += pack_nul(mrb, result, ridx, (int)count);
      continue;
    }
    else if (dir == PACK_DIR_BACK) {
      check_x(mrb, ridx, count, 'X');
      ridx -= count;
      continue;
    }
    else if (dir == PACK_DIR_ABS) {
      count -= ridx;
      if (count > 0) goto grow;
      count = -count;
      check_x(mrb, ridx, count, '@');
      ridx -= count;
      continue;
    }

    if ((flags & PACK_FLAG_WIDTH) && aidx >= RARRAY_LEN(ary)) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "too few arguments");
    }
    for (; aidx < RARRAY_LEN(ary); aidx++) {
      if (count == 0 && !(flags & PACK_FLAG_WIDTH))
        break;

      o = RARRAY_PTR(ary)[aidx];
      if (type == PACK_TYPE_INTEGER) {
        o = mrb_ensure_int_type(mrb, o);
      }
#ifndef MRB_NO_FLOAT
      else if (type == PACK_TYPE_FLOAT) {
        if (!mrb_float_p(o)) {
          o = mrb_ensure_float_type(mrb, o);
        }
      }
#endif
      else if (type == PACK_TYPE_STRING) {
        if (!mrb_string_p(o)) {
          mrb_raisef(mrb, E_TYPE_ERROR, "can't convert %T into String", o);
        }
      }

      /* Optimized dispatch using grouped format handling for better branch prediction */
      switch (dir) {
      /* Integer formats - all use (mrb, o, result, ridx, flags) signature */
      case PACK_DIR_CHAR:
        ridx += pack_char(mrb, o, result, ridx, flags);
        break;
      case PACK_DIR_SHORT:
        ridx += pack_short(mrb, o, result, ridx, flags);
        break;
      case PACK_DIR_LONG:
        ridx += pack_long(mrb, o, result, ridx, flags);
        break;
      case PACK_DIR_QUAD:
        ridx += pack_quad(mrb, o, result, ridx, flags);
        break;
      case PACK_DIR_BER:
        ridx += pack_BER(mrb, o, result, ridx, flags);
        break;

#ifndef MRB_NO_FLOAT
      /* Float formats - all use (mrb, o, result, ridx, flags) signature */
      case PACK_DIR_DOUBLE:
        ridx += pack_double(mrb, o, result, ridx, flags);
        break;
      case PACK_DIR_FLOAT:
        ridx += pack_float(mrb, o, result, ridx, flags);
        break;
#endif

      /* String formats with count - use (mrb, o, result, ridx, count, flags) signature */
      case PACK_DIR_HEX:
        ridx += pack_hex(mrb, o, result, ridx, (int)count, flags);
        break;
      case PACK_DIR_BSTR:
        ridx += pack_bstr(mrb, o, result, ridx, (int)count, flags);
        break;
      case PACK_DIR_STR:
        ridx += pack_str(mrb, o, result, ridx, (int)count, flags);
        break;

      /* String formats with count only - use (mrb, o, result, ridx, count) signature */
      case PACK_DIR_BASE64:
        ridx += pack_base64(mrb, o, result, ridx, (int)count);
        break;
      case PACK_DIR_UU:
        ridx += pack_uu(mrb, o, result, ridx, (int)count);
        break;
      case PACK_DIR_QENC:
        ridx += pack_qenc(mrb, o, result, ridx, (int)count);
        break;

      /* UTF8 format - special signature (mrb, o, result, ridx, count, flags) */
      case PACK_DIR_UTF8:
        ridx += pack_utf8(mrb, o, result, ridx, (int)count, flags);
        break;

      default:
        break;
      }
      if (flags & PACK_FLAG_COUNT2) {
        /* always consumes 1 entry */
        aidx++;
        break;
      }
      if (count > 0) {
        count--;
      }
    }
    if (ridx < 0) {
    overflow:
      mrb_raise(mrb, E_RANGE_ERROR, "negative (or overflowed) template size");
    }
  }

  mrb_str_resize(mrb, result, ridx);
  return result;
}

static mrb_value
pack_unpack(mrb_state *mrb, mrb_value str, mrb_bool single)
{
  struct tmpl tmpl;
  mrb_int count;
  unsigned int flags;
  enum pack_type type;
  mrb_int size;
  const unsigned char *sptr;

  prepare_tmpl(mrb, &tmpl);

  mrb_int srcidx = 0;
  mrb_int srclen = RSTRING_LEN(str);

  mrb_value result = mrb_ary_new(mrb);
  while (has_tmpl(&tmpl)) {
    enum pack_dir dir = read_tmpl(mrb, &tmpl, &type, &size, &count, &flags);

    if (dir == PACK_DIR_NONE) break;
    if (dir == PACK_DIR_NUL) {
      check_x(mrb, srclen-srcidx, count, 'x');
      srcidx += count;
      continue;
    }
    else if (dir == PACK_DIR_BACK) {
      check_x(mrb, srcidx, count, 'X');
      srcidx -= count;
      continue;
    }
    else if (dir == PACK_DIR_ABS) {
      check_x(mrb, srclen, count, '@');
      srcidx = count;
      continue;
    }

    /* Optimized dispatch for PACK_FLAG_COUNT2 formats - grouped by signature */
    sptr = (const unsigned char*)RSTRING_PTR(str) + srcidx;
    switch (dir) {
    /* String formats with count and flags - (mrb, sptr, len, result, count, flags) */
    case PACK_DIR_HEX:
      srcidx += unpack_hex(mrb, sptr, srclen - srcidx, result, (int)count, flags);
      if (single) goto single_return;
      continue;
    case PACK_DIR_BSTR:
      srcidx += unpack_bstr(mrb, sptr, srclen - srcidx, result, (int)count, flags);
      if (single) goto single_return;
      continue;
    case PACK_DIR_STR:
      srcidx += unpack_str(mrb, sptr, srclen - srcidx, result, (int)count, flags);
      if (single) goto single_return;
      continue;

    /* String formats without flags - (mrb, sptr, len, result) */
    case PACK_DIR_BASE64:
      srcidx += unpack_base64(mrb, sptr, srclen - srcidx, result);
      if (single) goto single_return;
      continue;
    case PACK_DIR_UU:
      srcidx += unpack_uu(mrb, sptr, srclen - srcidx, result);
      if (single) goto single_return;
      continue;
    case PACK_DIR_QENC:
      srcidx += unpack_qenc(mrb, sptr, srclen - srcidx, result);
      if (single) goto single_return;
      continue;

    default:
      break;
    }

    while (count != 0 && srcidx < srclen) {
      if (srclen - srcidx < size) {
        while (count-- > 0) {
          mrb_ary_push(mrb, result, mrb_nil_value());
        }
        break;
      }

      sptr = (const unsigned char*)RSTRING_PTR(str) + srcidx;
      /* Optimized dispatch for element-by-element formats - grouped by signature */
      switch (dir) {
      /* Integer formats - all use (mrb, sptr, len, result, flags) signature */
      case PACK_DIR_CHAR:
        srcidx += unpack_char(mrb, sptr, srclen - srcidx, result, flags);
        break;
      case PACK_DIR_SHORT:
        srcidx += unpack_short(mrb, sptr, srclen - srcidx, result, flags);
        break;
      case PACK_DIR_LONG:
        srcidx += unpack_long(mrb, sptr, srclen - srcidx, result, flags);
        break;
      case PACK_DIR_QUAD:
        srcidx += unpack_quad(mrb, sptr, srclen - srcidx, result, flags);
        break;
      case PACK_DIR_BER:
        srcidx += unpack_BER(mrb, sptr, srclen - srcidx, result, flags);
        break;

#ifndef MRB_NO_FLOAT
      /* Float formats - all use (mrb, sptr, len, result, flags) signature */
      case PACK_DIR_FLOAT:
        srcidx += unpack_float(mrb, sptr, srclen - srcidx, result, flags);
        break;
      case PACK_DIR_DOUBLE:
        srcidx += unpack_double(mrb, sptr, srclen - srcidx, result, flags);
        break;
#endif

      /* UTF8 format - uses (mrb, sptr, len, result, flags) signature */
      case PACK_DIR_UTF8:
        srcidx += unpack_utf8(mrb, sptr, srclen - srcidx, result, flags);
        break;

      default:
        mrb_raise(mrb, E_RUNTIME_ERROR, "mruby-pack's bug");
      }
      if (count > 0) {
        count--;
      }
    }
  }
  if (single) {
  single_return:
    if (RARRAY_LEN(result) > 0) {
      return RARRAY_PTR(result)[0];
    }
    return mrb_nil_value();
  }
  return result;
}

/*
 * call-seq:
 *   string.unpack(template) -> array
 *
 * Unpacks the contents of string according to the template string,
 * returning an array of values. Template uses the same format as
 * Array#pack. See Array#pack for template string format.
 *
 *   "\x01\x02\x03".unpack("CCC")     #=> [1, 2, 3]
 *   "\x01\x00\x02\x00".unpack("S*")  #=> [1, 2] (little endian)
 *   "hello     ".unpack("A10")       #=> ["hello"]
 */
static mrb_value
mrb_pack_unpack(mrb_state *mrb, mrb_value str)
{
  return pack_unpack(mrb, str, FALSE);
}

/*
 * call-seq:
 *   string.unpack1(template) -> object
 *
 * Unpacks the first value from string according to the template string.
 * This is equivalent to string.unpack(template)[0] but more efficient
 * when only the first value is needed.
 *
 *   "\x01\x02\x03".unpack1("C")      #=> 1
 *   "\x01\x00\x02\x00".unpack1("S")  #=> 1 (little endian)
 *   "hello     ".unpack1("A10")      #=> "hello"
 */
static mrb_value
mrb_pack_unpack1(mrb_state *mrb, mrb_value str)
{
  return pack_unpack(mrb, str, TRUE);
}

void
mrb_mruby_pack_gem_init(mrb_state *mrb)
{
  mrb_define_method_id(mrb, mrb->array_class, MRB_SYM(pack), mrb_pack_pack, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, mrb->string_class, MRB_SYM(unpack), mrb_pack_unpack, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, mrb->string_class, MRB_SYM(unpack1), mrb_pack_unpack1, MRB_ARGS_REQ(1));
}

void
mrb_mruby_pack_gem_final(mrb_state *mrb)
{
}
