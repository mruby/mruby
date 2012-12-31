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
 - S : 16-bit unsigned, native endian (uint16_t)
 - s : 16-bit signed, native endian (int16_t)
 - L : 32-bit unsigned, native endian (uint32_t)
 - l : 32-bit signed, native endian (int32_t)
 - n : 16-bit unsigned, network (big-endian) byte order
 - N : 32-bit unsigned, network (big-endian) byte order
 - A : arbitrary binary string (space padded, count is width)
 - a : arbitrary binary string (null padded, count is width)
 - Z : same as "a", except that null is added with *
 - H : hex string (high nibble first)
 - h : hex string (low nibble first)
 - m : base64 encoded string (see RFC 2045, count is width)

## License
This software is licensed under the same license terms of the original mruby.

