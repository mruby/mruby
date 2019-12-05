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

#ifndef __MICROUTF8_H__
#define __MICROUTF8_H__
#pragma once
#include <stdint.h>

/******************************************************************************
 * NOTE !!!                                                                   *
 *                                                                            *
 * ALL FUNCTIONS (EXCEPT utf8_check_validity()) EXPECT VALID UTF-8 STRING!    *
 ******************************************************************************/

/* Constants. */
#define UTF8_MICROUTF8_VERSION "1.2.0"
#define UTF8_VALID_SEQUENCE 1
#define UTF8_INVALID_SEQUENCE -1
#define NOPE_CHUCK_TESTA -1 /* Is it valid UTF-8 string? */
#define UTF8_NOT_ENOUGH_SPACE -2
#define UTF8_BOM "\xEF\xBB\xBF" /* note that it need to be casted to uint8_t* */

/* Functions. */

/*
utf8_strlen() takes null-terminated UTF-8 string as argument and returns number
of UTF-8 characters contained in string. Returns UTF8_INVALID_SEQUENCE if UTF-8
sequence is invalid. utf8_strlen() ignores BOM if present.
*/
int32_t utf8_strlen(uint8_t *str);

/*
utf8_get_nth_char() gives you requested n-th UTF-8 character from UTF-8 string.

utf8_get_nth_char() takes six arguments:
  str - null-terminated UTF-8 string
  target - string where requested character will be written (can be set to NULL)
  offset - pointer to integer where offset of character may be stored
           (can be set to NULL)
  is_bom_present - pointer to integer (can be set to NULL) which will be set to
                   1 if BOM is present in string or to 0 if there's no BOM
  n - number (from the left) of requested character
  max_length - maximal length (including trailing \0) of character in bytes.
               It can be set to 0 if target == NULL.

Return value:
  1 - success
  UTF8_INVALID_SEQUENCE - supplied UTF-8 sequence was invalid
  UTF8_NOT_ENOUGH_SPACE - max_length is too small
*/
int8_t utf8_get_nth_char(uint8_t *str, uint8_t *target, uint32_t *offset, uint8_t *is_bom_present, uint32_t n, uint8_t max_length);

/*
utf8_strip_bom() copies requested null-terminated UTF-8 string to another 
string without BOM, if there is no BOM resulting string will be exactly the
same.

utf8_strip_bom() takes three arguments:
  str - null-terminated UTF-8 string
  target - target string, if there is BOM (you can check it with 
           utf8_get_nth_char()) needed size will be strlen(str) - 2. If there
		   is no BOM needed size will be strlen(str) + 1.
  max_length - max length of new string.

Return value:
  1 - success
  UTF8_NOT_ENOUGH_SPACE - max_length is too small
*/

int8_t utf8_strip_bom(uint8_t *str, uint8_t *target, uint32_t max_length);

/*
utf8_check_validity() checks if supplied string is valid UTF-8 string. 

Return value:
  UTF8_VALID_SEQUENCE - supplied UTF-8 sequence was valid
  UTF8_INVALID_SEQUENCE - supplied UTF-8 sequence was invalid
*/


int8_t utf8_check_validity(uint8_t *str);

/*
utf8_is_bom_present() checks if supplied string contains UTF-8 BOM. 

Return value:
  1 - supplied UTF-8 string has UTF-8 BOM
  0 - there is no BOM
*/

int8_t utf8_is_bom_present(uint8_t *str);

const char *
utf8_decode (const char *o, uint32_t *val);

#endif
