#ifndef MRC_DUMP_H
#define MRC_DUMP_H

#include "mrc_irep.h"
#include "mrc_ccontext.h"

MRC_BEGIN_DECL

#define MRC_DUMP_DEBUG_INFO 1
#define MRC_DUMP_STATIC 2

#ifndef MRC_NO_STDIO
int mrc_dump_irep_cfunc(mrc_ccontext *c, const mrc_irep *irep, uint8_t flags, FILE *fp, const char *initname);
int mrc_dump_irep_binary(mrc_ccontext *c, const mrc_irep *irep, uint8_t flags, FILE* fp);
int mrc_dump_irep(mrc_ccontext *c, const mrc_irep *irep, uint8_t flags, uint8_t **bin, size_t *bin_size);
#endif

/* dump/load error code
 *
 * NOTE: MRC_DUMP_GENERAL_FAILURE is caused by
 * unspecified issues like malloc failed.
 */
#define MRC_DUMP_OK                     0
#define MRC_DUMP_GENERAL_FAILURE      (-1)
#define MRC_DUMP_WRITE_FAULT          (-2)
#define MRC_DUMP_READ_FAULT           (-3)
#define MRC_DUMP_INVALID_FILE_HEADER  (-4)
#define MRC_DUMP_INVALID_IREP         (-5)
#define MRC_DUMP_INVALID_ARGUMENT     (-6)

/* null symbol length */
#define MRC_DUMP_NULL_SYM_LEN         0xFFFF

/* Rite Binary File header */
#define RITE_BINARY_IDENT              "RITE"
/* Binary Format Version Major:Minor */
/*   Major: Incompatible to prior versions */
/*   Minor: Upper-compatible to prior versions */
#define RITE_BINARY_MAJOR_VER          "04"
#define RITE_BINARY_MINOR_VER          "00"
#define RITE_BINARY_FORMAT_VER         RITE_BINARY_MAJOR_VER RITE_BINARY_MINOR_VER
#if defined(RITE_COMPILER_NAME)
#undef RITE_COMPILER_NAME
#endif
#define RITE_COMPILER_NAME             "HSMK"
#define RITE_PARSER_NAME               "Prism"
#define RITE_COMPILER_VERSION          "0000"

#define RITE_VM_VER                    "0400"

#define RITE_BINARY_EOF                "END\0"
#define RITE_SECTION_IREP_IDENT        "IREP"
#define RITE_SECTION_DEBUG_IDENT       "DBG\0"
#define RITE_SECTION_LV_IDENT          "LVAR"

#define MRC_DUMP_DEFAULT_STR_LEN      128
#define MRC_DUMP_ALIGNMENT            sizeof(uint32_t)

/* binary header */
struct rite_binary_header {
  uint8_t binary_ident[4];    /* Binary Identifier */
  uint8_t major_version[2];   /* Binary Format Major Version */
  uint8_t minor_version[2];   /* Binary Format Minor Version */
  uint8_t binary_size[4];     /* Binary Size */
  uint8_t compiler_name[4];   /* Compiler name */
  uint8_t compiler_version[4];
};

/* section header */
#define RITE_SECTION_HEADER \
  uint8_t section_ident[4]; \
  uint8_t section_size[4]

struct rite_section_header {
  RITE_SECTION_HEADER;
};

struct rite_section_irep_header {
  RITE_SECTION_HEADER;

  uint8_t rite_version[4];    /* Rite Instruction Specification Version */
};

struct rite_section_debug_header {
  RITE_SECTION_HEADER;
};

struct rite_section_lv_header {
  RITE_SECTION_HEADER;
};

#define RITE_LV_NULL_MARK              UINT16_MAX

struct rite_binary_footer {
  RITE_SECTION_HEADER;
};

static inline size_t
mrc_uint8_to_bin(uint8_t s, uint8_t *bin)
{
  *bin = s;
  return sizeof(uint8_t);
}

static inline size_t
mrc_uint16_to_bin(uint16_t s, uint8_t *bin)
{
  *bin++ = (s >> 8) & 0xff;
  *bin   = s & 0xff;
  return sizeof(uint16_t);
}

static inline size_t
mrc_uint32_to_bin(uint32_t l, uint8_t *bin)
{
  *bin++ = (l >> 24) & 0xff;
  *bin++ = (l >> 16) & 0xff;
  *bin++ = (l >> 8) & 0xff;
  *bin   = l & 0xff;
  return sizeof(uint32_t);
}

static inline uint32_t
mrc_bin_to_uint32(const uint8_t *bin)
{
  return (uint32_t)bin[0] << 24 |
         (uint32_t)bin[1] << 16 |
         (uint32_t)bin[2] << 8  |
         (uint32_t)bin[3];
}

static inline uint16_t
mrc_bin_to_uint16(const uint8_t *bin)
{
  return (uint16_t)bin[0] << 8 |
         (uint16_t)bin[1];
}

static inline uint8_t
mrc_bin_to_uint8(const uint8_t *bin)
{
  return (uint8_t)bin[0];
}

static inline const char*
mrc_description(void)
{
  return "RITE" RITE_BINARY_FORMAT_VER " (" MRC_BUILD_INFO ") Parser: " RITE_PARSER_NAME "-" PRISM_VERSION;
}

MRC_END_DECL

#endif // MRC_DUMP_H

