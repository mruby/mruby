/*
Copyright (C) 2011 by Tomasz Konojacki

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#include "microutf8.h"
#include <string.h>

int32_t utf8_strlen(uint8_t *str)
{
	/* This function doesn't properly check validity of UTF-8 character 
	sequence, it is supposed to use only with valid UTF-8 strings. */
	
	int32_t character_count = 0;
	int32_t i = 0; /* Counter used to iterate over string. */
	uint8_t maybe_bom[4];
	
	/* If there is UTF-8 BOM ignore it. */
	if (strlen(str) > 2)
	{
		strncpy(maybe_bom, str, 3);
		maybe_bom[3] = 0;
		if (strcmp(maybe_bom, (uint8_t*)UTF8_BOM) == 0)
			i += 3;
	}
	
	while(str[i])
	{
		/* If bit pattern begins with 0 we have ascii character. */ 
		if (str[i] >> 7 == 0)
			++character_count;
		/* If bit pattern begins with 11 it is beginning of UTF-8 byte
		sequence. */
		else if (str[i] >> 6 == 3)
			++character_count;
		/* If bit pattern begins with 10 it is middle of utf-8 byte sequence. */
		else if (str[i] >> 6 == 2)
			;
		/* In any other case this is not valid UTF-8. */
		else
			return UTF8_INVALID_SEQUENCE;
		++i;
	}
	return character_count;	
}

int8_t utf8_get_nth_char(uint8_t *str, uint8_t *target, uint32_t *offset, uint8_t *is_bom_present, uint32_t n, uint8_t max_length)
{
	/* This function doesn't properly check validity of UTF-8 character 
	sequence, it is supposed to use only with valid UTF-8 strings. */
	uint8_t character_length = 0;
	int32_t character_count = 0;
	int32_t i = 0; /* Counter used to iterate over string. */
	int32_t _i = 0; /* Second counter, used to calculate size of UTF-8 character
	and to iterate over target character array */
	uint8_t maybe_bom[4];
	
	/* If there is UTF-8 BOM ignore it. */
	if (strlen(str) > 2)
	{
		strncpy(maybe_bom, str, 3);
		maybe_bom[3] = 0;
		if (strcmp(maybe_bom, (uint8_t*)UTF8_BOM) == 0)
		{
			/* There is BOM, set variable which is pointed by is_bom_present 
			to 1 */
			if (is_bom_present != NULL)
				*is_bom_present = 1;
			i += 3;
		}
		else
			if (is_bom_present != NULL)
				*is_bom_present = 0;
	}
	else
		if (is_bom_present != NULL)
			*is_bom_present = 0;
	
	while(str[i])
	{
		/* If bit pattern begins with 0 we have ascii character. */ 
		if (str[i] >> 7 == 0)
			if (++character_count == n)
			{
				/* That's the character that we were after. */
				character_length = 1;
				if (target != NULL)
				{
					if (max_length < 2)
						return UTF8_NOT_ENOUGH_SPACE;
					target[0] = str[i];
					target[1] = 0;
				}
				if (offset != NULL)
					*offset = i;
				return 1;
			}
		/* If bit pattern begins with 11 it is beginning of UTF-8 byte
		sequence. */
		else if (str[i] >> 6 == 3)
			if (++character_count == n)
			{
				/* That's the character that we were after. */
				character_length = 1;
				_i = i;
				/* Check size (in bytes) of character. */
				while (str[++_i] >> 6 == 2)
					++character_length;
				/* Check if there is enough space for character. */
				if (target != NULL)
					if (character_length + 1 > max_length)
						return UTF8_NOT_ENOUGH_SPACE;
				/* This sequence must be longer than one byte. */
				if (character_length == 1)
					return UTF8_INVALID_SEQUENCE;
				/* Write offset of character. */
				if (offset != NULL)
				{
					*offset = i;
				}
				/* Write character to target array. */
				if (target != NULL)
				{
					for (_i = 0; _i < character_length; ++_i)
						target[_i] = str[i++];
				/* End string which contains character with 0. */
					target[_i++] = 0;
				}
				return 1;		
			}
		/* If bit pattern begins with 10 it is middle of UTF-8 byte sequence. */
		else if (str[i] >> 6 == 2)
			;
		/* In any other case this is not valid UTF-8. */
		else
			return UTF8_INVALID_SEQUENCE;
		++i;
	}
	/* It should not happen. */
	return 0;
}

int8_t utf8_strip_bom(uint8_t *str, uint8_t *target, uint32_t max_length)
{
	int i = 0; /* Counter used to iterate over input string. */
	int _i = 0; /* Counter used to iterate over output string. */
	uint8_t maybe_bom[4];

	/* Skip BOM if present. */
	if (strlen(str) > 2)
	{
		strncpy(maybe_bom, str, 3);
		maybe_bom[3] = 0;
		if (strcmp(maybe_bom, (uint8_t*)UTF8_BOM) == 0)
		{
			/* Check if there is enough space. */
			if (strlen(str) - 2 > max_length)
				return UTF8_NOT_ENOUGH_SPACE;
			i += 3;
		}
		else 
			/* Check if there is enough space. */
			if (strlen(str) + 1 > max_length)
				return UTF8_NOT_ENOUGH_SPACE;
	}
	
	/* Write new string. */
	while (str[i])
	{
		target[_i] = str[i];
		++i;
		++_i;
	}

	/* Insert trailing 0. */
	target[_i] = 0;
	
	/* Probably success, return 1. */
	return 1;
}

int8_t utf8_check_validity(uint8_t *str)
{
	int i = 0; /* Counter used to iterate over input string. */
	int _i = 0; /* Counter used to iterate over character. */
	int charlen = 0; /* Current character length */
	uint8_t maybe_bom[4];

	/* Check BOM. */
	if (strlen(str) > 2)
	{
		strncpy(maybe_bom, str, 3);
		maybe_bom[3] = 0;
		if (strcmp(maybe_bom, (uint8_t*)UTF8_BOM) == 0)
			i += 3;
	}
	
	while (str[i])
	{
		/* If bit pattern begins with 0 we have ascii character. */ 
		if ((str[i] >> 7 == 0) && (_i == 0) && (charlen == 0))
			++i;
		else if ((str[i] >> 6 == 3) && _i == 0)
		{
			if (str[i] >> 5 == 6)
				/* Character sequence length equals 2. */
				charlen = 2;
			else if (str[i] >> 4 == 14)
				/* Character sequence length equals 3. */
				charlen = 3;
			else if (str[i] >> 3 == 30)
				/* Character sequence length equals 4. */
				charlen = 4;
			else if (str[i] >> 2 == 62)
				/* Character sequence length equals 5. */
				charlen = 5;
			else if (str[i] >> 1 == 126)
				/* Character sequence length equals 6. */
				charlen = 6;
			else
				/* This is not valid UTF-8 string. */
				return UTF8_INVALID_SEQUENCE;
			++i;
			++_i;
		}
		else if ((str[i] >> 6 == 2) && (charlen > 1))
		{
			if (charlen - 1 == _i)
			{
				/* This is last byte in character sequence, reset character
				counters. */
				charlen = 0;
				_i = 0;
				++i;
			}
			else
			{
				/* Continue iterating over string. */
				++_i;
				++i;
			}
		}
		else
			/* This is invalid UTF-8 string. */
			return UTF8_INVALID_SEQUENCE;
	}
	/* It seems that this is valid UTF-8 string. */
	return UTF8_VALID_SEQUENCE;
}

int8_t utf8_is_bom_present(uint8_t *str)
{
    uint8_t maybe_bom[4];
	if (strlen(str) > 2)
	{
		strncpy(maybe_bom, str, 3);
		maybe_bom[3] = 0;
		if (strcmp(maybe_bom, (uint8_t*)UTF8_BOM) == 0)
		{
			return 1;
		}
		else
            return 0;
    }
}

#define MAXUNICODE      0x10FFFF

/*
** Decode one UTF-8 sequence, returning NULL if byte sequence is invalid.
*/
const char *
utf8_decode (const char *o, uint32_t *val) {
  static const unsigned int limits[] = {0xFF, 0x7F, 0x7FF, 0xFFFF};
  const unsigned char *s = (const unsigned char *)o;
  unsigned int c = s[0];
  unsigned int res = 0;  /* final result */
  if (c < 0x80)  /* ascii? */
    res = c;
  else {
    int count = 0;  /* to count number of continuation bytes */
    while (c & 0x40) {  /* still have continuation bytes? */
      int cc = s[++count];  /* read next byte */
      if ((cc & 0xC0) != 0x80)  /* not a continuation byte? */
        return NULL;  /* invalid byte sequence */
      res = (res << 6) | (cc & 0x3F);  /* add lower 6 bits from cont. byte */
      c <<= 1;  /* to test next bit */
    }
    res |= ((c & 0x7F) << (count * 5));  /* add first byte */
    if (count > 3 || res > MAXUNICODE || res <= limits[count])
      return NULL;  /* invalid byte sequence */
    s += count;  /* skip continuation bytes read */
  }
  if (val) *val = res;
  return (const char *)s + 1;  /* +1 to include first byte */
}
