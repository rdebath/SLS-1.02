/*
 * A client for term.
 */
#define I_SYS
#define I_IOCTL
#define	I_TTY
#include "includes.h"
#include "client.h"
#include <signal.h>

int debug = 0; 
int term_client_number = -1;
int s;
int simple = 0;

char * pathtail(char*);

static int local_options ( char opt, char *optarg )
{
  switch(opt)
  {
  case 's' :
    simple = 1; 
    break;
  default:
    return -1;
  }
  return 0;
}

/* tty clean up and exit */
void quit(int ret)
{
  set_block(s);
  set_block(0);
  set_block(1);

  if (!simple && isatty(0))
  {
    terminal_restore(0);
    terminal_restore(1);
  }

  exit (ret);
}

	/* In case of sudden death, do some */
	/* minimal clean up. */
void sig_quit(int dummy) 
{
  quit(1);
}

#ifdef USE_SIGWINCH
void resizer(int foo)
{
#ifdef TIOCGWINSZ
  struct winsize ws;
  if (ioctl(0, TIOCGWINSZ, &ws) >= 0)
#else	/* TIOCGWINSZ */
#ifdef TIOCGSIZE
  struct ttysize ts;
  if (ioctl(0, TIOCGSIZE, &ts) >= 0)
#endif
#endif
  {
    int s = connect_server(term_server);
    if (!s)  return;  /* cannot connect for some reason */
#ifdef TIOCGWINSZ
    send_command(s, C_RESIZE, 0, "%d %d %d %d %d", term_client_number,
		 ws.ws_row, ws.ws_col, ws.ws_ypixel, ws.ws_xpixel);
#else	/* TIOCGWINSZ */
#ifdef TIOCGSIZE
    send_command(s, C_RESIZE, 0, "%d %d %d %d %d", term_client_number,
                 ts.ts_lines, ts.ts_cols, ts.ts_yyy, ts.ts_xxx);
#endif
#endif
    close(s);
  }
  signal(SIGWINCH, resizer);
}
#endif	/* USE_SIGWINCH */

void main(int argc, char *argv[]) {
  int s, first;
  char * f;
  
  priority = 2;
  
  first = client_options(argc,argv,"s", local_options);
  if (first >= argc)  simple = 0;
  
  s = connect_server(term_server);
  if (s < 0 )  exit(1);  /* shouldn't ever happen since connect_server() exits */

  if (send_command(s, C_STATS, 1, "%d", -6)<0) {
    fprintf(stderr, "FATAL: Failed to get client number.\n");
    fprintf(stderr, "Reason given: %s\n", command_result);
    exit(1);
  }

#ifdef USE_SIGWINCH
  term_client_number = atoi(command_result);
  if (term_client_number < 0)
    fprintf(stderr, "Can't get client number: SIGWINCH not supported.\n");
#endif

  if (send_command(s, simple ? C_EXEC : C_PTYEXEC, 0, "%s", f = build_arg(&argv[first]))< 0) {
    fprintf(stderr, "FATAL: Failed to exec remote command\n");
    fprintf(stderr, "Reason given: %s\n", command_result);
    exit(1);
  }
  free(f);
  send_command(s, C_DUMB, 1, 0);

#ifdef USE_SIGWINCH  
  if (!simple && isatty(0))
    resizer(0);
#endif
  
  terminal_save(0);

  signal(SIGHUP, sig_quit);
  signal(SIGINT, sig_quit);
  signal(SIGQUIT, sig_quit);
  signal(SIGPIPE, sig_quit);

  if (!simple && isatty(0))
  {
    terminal_raw(0);
    terminal_raw(1);  
  }
  set_nonblock(0);
  set_nonblock(1);
  set_nonblock(s);

  do_select_loop(s, 0, 1);

  quit (0);
}


/* my function.
 * char * pathtail( char * path )
 * takes a path and returns a pointer to the last element in it
 * assumes path separator is '/'
 * assumes '\0' terminated path.
 *
 * note: returns a pointer to the string that was input. 
 * note: returns NULL if path == NULL.
 * 
 *
 * by croutons. 29 Nov 1992
 */

char * pathtail(char * p)
{
	int i;

	if ( ! p ) { 
		return NULL;
	}

	for (i = 0; p[i] != '\0'; i++); 
	for (i--; i >= 0 && p[i] != '/'; i--);
	return &p[i+1];
}
