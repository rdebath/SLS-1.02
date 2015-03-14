
#include "terminal.h"

int read_into_buff(int fd, char *b, int *s, int *e, int size);
int write_from_buff(int fd, char *b, int *s, int *e, int size);

int do_read_into_buff(int fd, char *b, int *s, int *e, int size);
int do_write_from_buff(int fd, char *b, int *s, int *e, int size);

int connect_server(char *);

void set_nonblock(int);
void set_block(int);

void set_ttyraw(int);
void set_ttynormal(int);

void lib_init(int *argc, char *argv[]);

extern char *term_server;
