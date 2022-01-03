# hs-mmap-mutablebytearray

This repo contains a minimum example of how combining `mmap` with page aligned and
pinned `MutableByteArray#`s leads to a segfault.

It's not clear, to me, if the bug is in this repo or in GHC.

Tested with latest GHC available in `ghcup` at the time, which is GHC version
9.2.1.

The following steps should let you reproduce the segfault locally:

```bash
git clone https://github.com/stevana/hs-mmap-mutablebytearray.git
cd hs-mmap-mutablebytearray
make
make ulimit # enable core dumps
make strace # run the program under strace
make gdb    # debug the core dump
```

Some observations:

* `mmap` by itself doesn't seem to be the problem, see the `worksMmap` example
  in the `Mmap` module, and run it with `make works`. This example uses the
  `posix_memalign` syscall to allocate page size aligned memory (it needs to be
  page aligened for `mmap` to work, and the `Foregin` modules don't provide a
  way to do that as far as I could tell.)
