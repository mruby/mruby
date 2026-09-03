# mruby-socket

"mruby-socket" mrbgem provides BSD socket interface for mruby.
API is compatible with CRuby's "socket" library.

## Example

```sh
% vi kame.rb
s = TCPSocket.open("www.kame.net", 80)
s.write("GET / HTTP/1.0\r\n\r\n")
puts s.read
s.close

% mruby kame.rb
HTTP/1.1 200 OK
Date: Tue, 21 May 2013 04:31:30 GMT
...
```

## Requirement

- [mruby-io](../mruby-io) mrbgem
- [iij/mruby-mtest](https://github.com/iij/mruby-mtest) mrbgem to run tests
- system must have RFC3493 basic socket interface
- and some POSIX API...

## TODO

- add missing methods
- write more tests
- fix possible descriptor leakage (see XXX comments)
- `UNIXSocket#recv_io` `UNIXSocket#send_io`

## What the port declares

Whether a method exists is the port's to say, since the port is what a build
names and a `hal-socket-<conf>` gem may stand in for the bundled ones. Each
port publishes a `socket_hal_features.h` in its `include/`, which
`include/socket_hal.h` reads before it declares anything. One macro there
guards the prototype, the port's implementation and the method definition, so
a capability the port does not declare has no method at all rather than one
that fails, and `respond_to?` answers false. A port that declares a capability
it does not implement fails to link.

| macro                            | methods                                    | posix                    | win |
| -------------------------------- | ------------------------------------------ | ------------------------ | --- |
| `MRB_HAL_SOCKET_HAS_SOCKADDR_UN` | `Socket.sockaddr_un`, `Addrinfo#unix_path` | o                        |     |
| `MRB_HAL_SOCKET_HAS_SOCKETPAIR`  | `Socket.socketpair`                        | o                        |     |
| `MRB_HAL_SOCKET_HAS_FD_IO`       | `BasicSocket#sysseek`                      | o                        |     |
| `MRB_HAL_SOCKET_HAS_GETPEEREID`  | `BasicSocket#getpeereid`                   | o, on macOS and the BSDs |     |

`MRB_HAL_SOCKET_HAS_FD_IO` guards no port function. It says a socket is a
file descriptor that read(2), write(2) and lseek(2) take, so `BasicSocket`
inherits every method of `IO`; a port that leaves it out gets `sysread`,
`syswrite`, `read` and `write` through recv(2) and send(2) instead, and no
`sysseek`. That is all it changes. A socket that is not a descriptor still
has to be one `IO.new` accepts and `IO#close` closes as a socket, and both
of those are mruby-io's, which today knows a Winsock handle and no other;
a port for another such host brings that knowledge to mruby-io before it
leaves this macro out.

The Ruby layer is not gated. `UNIXSocket.new`, `UNIXServer.new`,
`UNIXSocket.socketpair` and `Socket.unpack_sockaddr_un` keep their bodies on
every port and reach the C method they build on, whose refusal is what a
caller sees.

## License

Copyright (c) 2013 Internet Initiative Japan Inc.
Copyright (c) 2017 mruby developers

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
