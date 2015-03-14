#define I_IOCTL
#define I_ERRNO

#include "includes.h"

#include "debug.h"

int term_errno = 0;
/*-----------------------------------------------------------------------*/
void set_nonblock(int fd) {
#ifdef USE_FCNTL
#ifdef USE_NONBLOCK
  if (!(fcntl (fd, F_GETFL) & O_NONBLOCK)) {
    fcntl (fd, F_SETFL, (fcntl(fd, F_GETFL) | O_NONBLOCK));
  }
#else
  if (!(fcntl (fd, F_GETFL) & FNDELAY)) {
    fcntl (fd, F_SETFL, (fcntl(fd, F_GETFL) | FNDELAY));
  }
#endif
#else
  int one = 1;
  ioctl( fd, FIONBIO, &one);
#endif
}

void set_block(int fd) { 
#ifdef USE_FCNTL
#ifdef USE_NONBLOCK
  fcntl (fd, F_SETFL, (fcntl(fd, F_GETFL) & ~O_NONBLOCK));
#else
  fcntl (fd, F_SETFL, (fcntl(fd, F_GETFL) & ~FNDELAY));
#endif
#else
  int zero = 0;
  ioctl(fd, FIONBIO, &zero);
#endif
}
/*------------------------------------------------------------------------*/
/* Do a partial read into the buffer. */
int do_read_into_buff(int fd, struct Buffer *b, int size) {
  int r = 0, t;
  
  if (!size || size > BUFFER_SIZE - 1 - b->size)
    size = BUFFER_SIZE - 1 - b->size;
  term_errno = 0;
  t = BUFFER_SIZE - b->start;
  if (t > size) t = size;
  r = read(fd, b->data + b->start, t);
  DEBUG_LL(stderr, "%s: d_r_i_b: read1 from %d (%d) did %d\n", 
	   term_server, fd, t, r);
  if (r <= 0)  {
    if (!r) term_errno = 1;
    else 
#ifndef SVR4
      if (errno != ERR_BLOCK)
#endif /* SVR4 */
        term_errno = errno + 1;
    return r;
  }
  
  b->start += r;
  b->size += r;
  SANITY(b->start <= BUFFER_SIZE);
  if (b->start == BUFFER_SIZE)
    b->start = 0;
  size -= r;
  if (!size || b->start != 0) return r;
  
  t = BUFFER_SIZE - b->start;
  if (t > size) t = size;
  t = read(fd, b->data + b->start, t);
  DEBUG_LL(stderr, "%s: d_r_i_b: read2 from %d (%d) did %d\n", 
	   term_server, fd, t, t);
  if (t <= 0) {
    if (!t) term_errno = 1;
    else if(errno != ERR_BLOCK)
      term_errno = errno+1;
    return r;
  }
  
  b->start += t;
  b->size += t;
  if (b->start == BUFFER_SIZE)
    b->start = 0;
  size -= t;
  r += t;
  return r;
}

/* Read from a file-descriptor into a ring buffer. Read atmost size bytes */
int read_into_buff(int fd, struct Buffer *b, int size) {
  int ret, l = 0;
  if (!size) size = BUFFER_SIZE - 1 - b->size;

  do {
    ret = do_read_into_buff(fd, b, size);
    if (ret < 1) break;
    l += ret;
    size -= ret;
  } while (!term_errno && size);
  if (!l) return ret;
  return l;
}

/*------------------------------------------------------------------------*/
/* Write from ring buffer to file descriptor. Write at most size bytes    */

int write_from_buff(int fd, struct Buffer *b, int size) {
  int r = 0, t;
  if (!size) size = b->size;
  t = BUFFER_SIZE - b->end;
  if (t > size) t = size;
  
  r = write(fd, b->data + b->end, t);
  DEBUG_LL(stderr, "%s: d_w_f_b: write1 from %d (%d) did %d\n", 
	   term_server, fd, t, r);
  if (r <= 0) {
    if (!r) term_errno = 1;
    else if (errno != ERR_BLOCK)
      term_errno = errno+1;
    return r;
  }

  b->end += r;
  b->size -= r;
  SANITY(b->end <= BUFFER_SIZE);

  if (b->end >= BUFFER_SIZE)
    b->end -= BUFFER_SIZE;
  size -= r;
  if (!size || b->end != 0) 
    return r;
  
  t = BUFFER_SIZE - b->end;
  if (t > size) t = size;
  t = write(fd, b->data + b->end, t);
  DEBUG_LL(stderr, "%s: d_w_f_b: write2 from %d (%d) did %d\n", 
	   term_server, fd, t, r);

  if (t <= 0) {
    if (!t) term_errno = 1;
    else if (errno != ERR_BLOCK)
      term_errno = errno+1;
    return r;
  }
  
  b->end += t;
  b->size -= t;
  SANITY(b->end <= BUFFER_SIZE);
  if (b->end >= BUFFER_SIZE)
    b->end -= BUFFER_SIZE;
  r += t;
  return r;
}
				/* Should check for overflow here.. */
int add_to_buffer(struct Buffer *b, un_char c) {
  b->data[b->start++] = c;
  b->size++;
/*
  if (b->size >= BUFFER_SIZE - 2)
    fprintf (stderr, "add_to_buffer() buffer overflow %d\n", b->size);
*/  
  if (b->start >= BUFFER_SIZE)
    b->start -= BUFFER_SIZE;
  return 1;
}

int get_from_buffer(struct Buffer *b) {
  un_char c;
  if (b->size < 1)
    return -1;
  c = b->data[b->end++];
  --b->size;
  if (b->end == BUFFER_SIZE)
    b->end = 0;
  return c&255;
}
/*-----------------------------------------------------------------------*/

#ifdef sun
char *strerror(int errno) {
  extern char *sys_errlist[];
  
  return sys_errlist[errno];
}
#endif
