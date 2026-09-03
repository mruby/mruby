# mruby-dir

Dir class for mruby. Supported methods are:

`.chdir`
`.chroot`
`.delete`
`.entries`
`.exist?`
`.foreach`
`.getwd`
`.mkdir`
`.open`
`#close`
`#each`
`#read`
`#rewind`
`#seek`
`#tell`

## What the port declares

Whether a method exists is the port's to say, since the port is what a build
names and a `hal-dir-<conf>` gem may stand in for the bundled ones. Each port
publishes a `dir_hal_features.h` in its `include/`, which
`include/dir_hal.h` reads before it declares anything. One macro there guards the prototype, the
port's implementation and the method definition, so a capability the port does
not declare has no method at all rather than one that fails, and
`respond_to?` answers false. A port that declares a capability it does not
implement fails to link.

| macro                    | methods      | posix                    | win |
| ------------------------ | ------------ | ------------------------ | --- |
| `MRB_HAL_DIR_HAS_SEEK`   | `Dir#seek`   | o, not on Android        |     |
| `MRB_HAL_DIR_HAS_TELL`   | `Dir#tell`   | o, not on Android        |     |
| `MRB_HAL_DIR_HAS_CHROOT` | `Dir.chroot` | o, not on Android or DOS |     |

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

### On Windows platforms, you must agree on additional license too:

Copyright Kevlin Henney, 1997, 2003, 2012. All rights reserved.

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose is hereby granted without fee, provided
that this copyright and permissions notice appear in all copies and
derivatives.

This software is supplied "as is" without express or implied warranty.

But that said, if there are any problems please get in touch.
