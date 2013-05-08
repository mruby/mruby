mruby-pack (pack / unpack)
=========

## install by mrbgems
```bash
git clone git://github.com/iij/mruby-pack.git
cp -pr mruby-pack ${MRUBY_ROOT}/mrbgems/g/.
echo mruby-pack >> ${MRUBY_ROOT}/mrbgems/GEMS.active
cd ${MRUBY_ROOT}
rake ENABLE_GEMS="true"
./bin/mruby ${MRUBY_ROOT}/mrbgems/g/mruby-pack/example/sample.rb
```

## support template string
 - C : 8-bit unsigned (unsigned char)
 - c : 8-bit signed (signed char)
 - E : 64-bit double, little endian
 - S : 16-bit unsigned, native endian (uint16_t)
 - s : 16-bit signed, native endian (int16_t)
 - L : 32-bit unsigned, native endian (uint32_t)
 - l : 32-bit signed, native endian (int32_t)
 - n : 16-bit unsigned, network (big-endian) byte order
 - N : 32-bit unsigned, network (big-endian) byte order
 - v : 16-bit unsigned, VAX (little-endian) byte order
 - V : 32-bit unsigned, VAX (little-endian) byte order
 - A : arbitrary binary string (space padded, count is width)
 - a : arbitrary binary string (null padded, count is width)
 - Z : same as "a", except that null is added with *
 - H : hex string (high nibble first)
 - h : hex string (low nibble first)
 - m : base64 encoded string (see RFC 2045, count is width)


## License

Copyright (c) 2012 Internet Initiative Japan Inc.

Permission is hereby granted, free of charge, to any person obtaining a 
copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in 
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
DEALINGS IN THE SOFTWARE.

