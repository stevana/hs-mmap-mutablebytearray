#include <sys/mman.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

int step(void) {
  int fd;
  char *addr;

  fd = open("/tmp/mmap.txt", O_RDWR);
  if (fd == -1) {
    return(1);
  } else {
    // printf("opened file, fd: %d\n", fd);
  }

  const int pagesize = getpagesize();

  static char* buf;
  const int res = posix_memalign((void **)&buf, pagesize, pagesize);
  if (res != 0)
    printf("posix_memalign failed\n");

  printf("pagesize: %d\n", pagesize);

  addr = mmap(buf, pagesize, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, fd, 0);
  if (addr == MAP_FAILED) {
    printf("mmap 2 failed\n");
    return(2);
  }
  munmap(addr, pagesize);
  free(buf);
  close(fd);
}

int main (void) {

  int res;
  printf("_SC_PAGESIZE: %d\n", _SC_PAGESIZE);

  int i;
  for (i = 0; i < 1000000; i++) {
    if (i % 1000 == 0)
      printf("iteration: %d\n", i);
    res = step();
    if (res != 0) {
      printf("step failed: %d\n", res);
    }
  }


    /*
  printf("addr*: %p\n", (void *) addr);

  strncpy(buf, "Bello", 6);
  strncpy(addr, "Aello", 6);

  printf("buf: %s\n", buf);
  printf("addr: %s\n", addr);
    */

}