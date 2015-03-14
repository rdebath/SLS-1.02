#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef LATTICE
#include <proto/exec.h>
#endif
#include "config.h"

Prototype char *mktemp(char *buf);

char *
mktemp(char *buf)
{
  long pid = (long)FindTask(0L);
  char *c;

  c = buf + strlen(buf);

  while (*--c == 'X') {
    *c = pid % 10 + '0';
    pid /= 10;
  }
  c++;
  if (*c) {
    for(*c='A'; *c <= 'Z'; (*c)++) {
      if (access(buf, 0)) {
        return buf;
      }
    }
    *c = 0;
    return buf;
  }else{
    return buf;
  }
}
