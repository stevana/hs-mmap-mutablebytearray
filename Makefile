all:
	hsc2hs Mmap.hsc && ghc -debug -g --make ./Main.hs && ./Main +RTS -DS

ulimit:
	ulimit -c unlimited

strace:
	strace ./Main +RTS -DS

gdb:
	gdb ./Main ./core
