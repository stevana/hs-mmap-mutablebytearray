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

* Depending on how big we make the `mmap`ed `MutableByteArray#` we get segfaults
  at different places. If the bytearray is of the same size as the page size (as
  is the case when run with `make`), then `gdb` and `backtrace` tell us that the
  segfault is caused by:
  https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/sm/BlockAlloc.c#L833 .
  Whereas if the size of the byte array is smaller (as in the case when run with
  `make small`), e.g. 16 bytes, then `backtrace` in `gdb` mentions
  `stg_newAlignedPinnedByteArrayzh`:
  https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/PrimOps.cmm#L113 .

After asking in #haskell and #ghc on IRC the following was pointed out to me:

* A similar request about being able to `mmap` byte arrays has been
  discussed in the following ticket:

    https://gitlab.haskell.org/ghc/ghc/-/issues/17747 

* The problems can perhaps be avoided by first `mmap`ing a `Ptr`
  then using `fetchAndWordAddr#` rather than working with byte arrays.
