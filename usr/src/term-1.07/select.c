#define I_SYS
#define I_ERRNO
#include "includes.h"

void do_select_loop(int sock, int in_f, int out_f) {
  struct Buffer in = {{0},0,0,0}, out = {{0},0,0,0};
  int i;

  in.start = in.end = in.size = 0;
  out.start = out.end = out.size = 0;
  
  while (1) {
    fd_set rd, wr;
    int max;
    max = 0;
    FD_ZERO(&rd);
    FD_ZERO(&wr);
    if (in.size) {
      FD_SET(sock, &wr);
      if (sock  > max) max = sock;
    } else {
      FD_SET(in_f, &rd);
      if (in_f > max) max = in_f;
    }
    
    if (out.size) {
      FD_SET(out_f, &wr);
      if (out_f > max) max = out_f;
    } else {
      FD_SET(sock, &rd);
      if(sock > max) max = sock;
    }
    
    select(max+1, &rd, &wr, 0, 0);
    
    if (FD_ISSET(out_f, &wr)) {
      i = write_from_buff(out_f, &out, 0);
      if (i <1 && term_errno)
	break;
    }
    if (FD_ISSET(sock, &wr)) {
      i = write_from_buff(sock, &in, 0);
      if (i<1 && term_errno)
	break;
    }
    
    if (FD_ISSET(in_f, &rd)) {
      i = read_into_buff(in_f, &in, 0);
      if (i<1 && term_errno)
	break;
    }
    
    if (FD_ISSET(sock, &rd)) {
      i = read_into_buff(sock, &out, 0);
      if (i<1 && term_errno)
	break;
    }
  }
}



