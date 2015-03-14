#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>

double getload(void)
{
  static int init=0, fd;
  char buf[10];

  if (!init)
  {
    fd=open("/proc/loadavg",O_RDONLY);
    if (fd<0) { perror("Can't open /proc/loadavg"); exit(1); }
    init=1;
  }
  lseek(fd,(off_t)0,SEEK_SET);
  read(fd,buf,sizeof(buf)); buf[sizeof(buf)-1]='\0';
  return (atof(buf));
}
