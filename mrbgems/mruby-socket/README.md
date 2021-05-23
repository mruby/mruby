mruby-socket
============

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

* [mruby-io](https://github.com/mruby/mruby/tree/master/mrbgems/mruby-io) mrbgem
* [iij/mruby-mtest](https://github.com/iij/mruby-mtest) mrgbem to run tests
* system must have RFC3493 basic socket interface
* and some POSIX API...

## TODO

* add missing methods
* write more tests
* fix possible descriptor leakage (see XXX comments)
* `UNIXSocket#recv_io` `UNIXSocket#send_io`

## License

The MIT License. See [LICENSE](LICENSE) for more details.
