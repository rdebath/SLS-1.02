#ifndef CLIENT_H
#define CLIENT_H
#include "config.h"
#include "terminal.h"

#ifndef un_char
#define un_char unsigned char
#endif
#define BUFFER_SIZE 2048

struct Buffer {
    un_char data[BUFFER_SIZE];
    int start, end;
    int size;
    };

#define C_SWITCH        '@'

#define C_OPEN		'1'	/* Open a file a file for uploading */
				/* without truncating it. */
#define	C_CLOSE		'2'	/* Close a connection. */
#define C_EXEC		'3'	/* Not used/implemented. */
#define C_PTYEXEC	'4'	/* Fork() a shell attached to a pty. */
#define C_DUMB		'5'	/* Go dumb. Escape all '@' received, */
				/* so no commands are processed. */
#define C_UPLOAD	'6'	/* Open a file for writing, creating */
				/* it if it doesn't exist, truncating */
				/* it if it does. */

#define C_DOWNLOAD	'7'	/* Open a file for reading. */
#define C_SOCKET        '8'	/* Connect to a unix-domain socket. */
#define C_PORT          'A'	/* Connect to a TCP/IP port on the */
				/* specified host, and port number. */
#define C_PRIORITY      'B'	/* Raise/lower the priority of this */
				/* client. */
#define C_COMPRESS	'C'  /* note, i jumped B :) (croutons). */
				/* Turn compression on or off. */
#define C_STAT          'D'	/* Get information on a remote file. */
#define C_SEEK          'E'	/* Execute an lseek() on the remote */
				/* file descriptor. Assumes the remote */
				/* is a file handle, and not a socket. */
#define C_STATS         'F'	/* Get information on various parts of */
				/* term.  */
#define C_NAME          'G'	/* Set the name of this client. Only */
				/* used by C_STATS. */
#define C_RESIZE        'H'	/* For handleing SIGWINCH */
#define C_CLIENTNUM     'I'	/* Not used. Obselete. */
#define C_BIND          'J'	/* Bind a remote socket. */
#define C_ACCEPT        'K'	/* Accept a connection from a remotely */
				/* bound sockets. */
#define C_DUMP          'L'	/* Go dumb for the next 'n' bytes. */
#define C_CLCLOSE       'M'     /* Close the remote fd when the */
				/* buffers have been emptied. */
#define C_QUIT          'N'	/* Shutdown term. */

#define I_FAIL		'a'
#define I_CLOSE		'b'
#define I_EXIT		'c'
#define I_OK		'd'

int read_into_buff(int fd, struct Buffer *, int);
int write_from_buff(int fd, struct Buffer *, int);

int do_read_into_buff(int fd, struct Buffer *, int);

int connect_server(char *);

void set_nonblock(int);
void set_block(int);

void set_ttyraw(int);
void set_ttynormal(int);

typedef int (*Callback) ( char, char *);
int client_options(int argc, char *argv[], char *myopts, Callback callback);
int send_command(int, int, int, char *, ...);
char * build_arg(char**);
extern int priority;

int open_unix(char *);
int bind_tcp( int );
int bind_unix(char *);
void do_select_loop(int, int, int);
extern char *term_server;

extern char *command_result;

void do_connect(int num, int *svs, int (*get_server)(void));
#endif
