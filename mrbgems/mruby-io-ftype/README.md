# mruby-io-ftype

`File.ftype` and the `FileTest` predicates that mruby-io leaves out, for a
build that asks for them:

```ruby
conf.gem core: 'mruby-io-ftype'
```

It adds these to both `File` and `FileTest`, except `ftype`, which only
`File` has:

| method             | answers                                           |
| ------------------ | ------------------------------------------------- |
| `ftype`            | `"file"`, `"directory"`, `"link"` and the others  |
| `blockdev?`        | a block device                                    |
| `chardev?`         | a character device                                |
| `empty?`           | there and has no bytes, as `zero?`                |
| `readable_real?`   | readable by the real user and group               |
| `writable_real?`   | writable by the real user and group               |
| `executable_real?` | executable by the real user and group             |
| `setuid?`          | the setuid bit is set                             |
| `setgid?`          | the setgid bit is set                             |
| `sticky?`          | the sticky bit is set                             |
| `world_readable?`  | the permission bits when others may read, or nil  |
| `world_writable?`  | the permission bits when others may write, or nil |
| `owned?`           | owned by the effective user                       |
| `grpowned?`        | its group is one of the process's                 |
| `identical?`       | the two name the same file                        |

`ftype` does not follow a symbolic link and raises for a file that is not
there; the predicates answer false (or nil) for one. Each takes an IO as well
as a path.

`owned?` and `grpowned?` are there when the mruby-io port declares
`MRB_HAL_IO_HAS_OWNER`, and `identical?` when it declares
`MRB_HAL_IO_HAS_STAT_INODE`. The POSIX port declares both and the Windows
port neither.

Originally written by Akira Matsuda (mruby/mruby#7817).
