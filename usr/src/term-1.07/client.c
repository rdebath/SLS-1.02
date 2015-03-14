#define I_SYS
#define I_GETOPT
#include "includes.h"
#ifdef USE_VARARGS
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#include <string.h>

#include "client.h"

char *term_server = "";

int lcompression = -1;		/* defaults. */
int rcompression = -1;
int priority = 0;
char com_result[200];
char *command_result;

				/* parse all the options, and then */
				/* return the first unused argument. */
int client_options(int argc, char *argv[], char *myopts, Callback callback)
{
  int c;
  char args[200];

  /* make sure we get all the args to pass to getopt */
  if (getenv("TERMSERVER"))
    term_server = getenv("TERMSERVER");

  strcpy(args, "t:crp:");
  strcat(args, myopts);

  while ((c = getopt(argc, argv, args))!=EOF) {
    switch(c) {
    case 't':
      term_server = optarg;	
      break;
    case 'c': /* for compressionion */
      lcompression = 1;
      rcompression = 1;
      break;
    case 'r': /* for raw, no compressionion */
      lcompression = 0;
      rcompression = 0;
      break;
    case 'p':
      priority = atoi(optarg);
      fprintf(stderr, "Changeing priority to %d\n", priority);
      break;
    case '?':
      return -1;
      break;
    default:
      if ( (callback == NULL) || ( callback(c, optarg) == -1 ) )
	{
	  fprintf(stderr, "Unrecognized option %s\n", argv[optind]);
	  return -1; 
	}
      break;
    }
  }
  return optind;
}
				/* Send a command to the Term */
				/* process.*/ 
#ifdef USE_VARARGS
int send_command(sock , comm, local , fmt, va_alist)
int sock , comm, local;
char *fmt;
va_dcl {
#else
int send_command(int sock, int comm, int local, char *fmt, ...) {
#endif
  char buff[200];
  va_list v;
  int i;
#ifdef USE_VARARGS
  va_start(v);
#else
  va_start(v, fmt);
#endif
				/* A command follows.. */
  buff[0] = C_SWITCH;
				/* Is it for the local term? or the */
				/* remote term? */
  if (local)
    buff[1] = C_SWITCH - 1;
  else
    buff[1] = C_SWITCH - 2;
				/* Set the command in. */
  buff[2] = comm;
  if (fmt)
    vsprintf(buff+3, fmt, v);
  else
    buff[3] = 0;

  va_end(v);
				/* Ok. Buff holds the entire command. */
				/* Lets do the damned thing.. */
  if (write(sock, buff, strlen(buff)+1) < 0)
    return -1;
				/* If the command is C_DUMB, then no */
				/* reply will be forthcoming. */
  if (comm == C_DUMB) return 1;

  i = 0;
  do {
    read(sock, &com_result[i], 1);
  } while (com_result[i++]);
  command_result = &com_result[3];

  if (com_result[2] != I_OK)
    return -1;
/*  printf("command return was (%d)%s.\n", i, com_result);*/
  return 1;
}

int connect_server(char *server) {
  char path[200];
  char *home;
  int s;
  
  home = getenv("TERMDIR");
  if (!home)
    home = getenv("HOME");
  if (!home)
    home = "/tmp";   
  if (!server) server = "";

  sprintf(path,"%s/.term/socket%s", home, server);

				/* Try and connect to term. */
  s = open_unix(path);
  if (s < 0)  exit(1);

  if ( lcompression >=0 )
    send_command(s, C_COMPRESS, 1, "%d", lcompression);
  if ( rcompression >=0 )
    send_command(s, C_COMPRESS, 0, "%d", rcompression);

  if (priority) {
    send_command(s, C_PRIORITY, 1, "%d", priority);
    send_command(s, C_PRIORITY, 0, "%d", priority);
  }

  return s;
}

/* build_arg: build a string from char ** argv to be passed 
 * to C_EXEC family. 
 *
 * by: croutons
 *
 * Notes:
 *  returns a pointer to malloced space.
 *  takes a null pointer as the end of the array of char*.
 *  assumes null terminated strings. (for using string(3))
 *  returns NULL on error.
 *
 *  we assume '\xff' is ok for the new terminator
 */
char * build_arg( char**arg )
{
	int i, s;
	char * f;

	if ( ! arg ) return NULL;
	for ( s = i = 0; NULL != arg[i]; i++) s += strlen(arg[i]);
	s += i + 2;
	if ( NULL == (f = (char *)malloc( s * sizeof(char) ) ) ) {
		return NULL;
	}
	for ( s = i = 0; NULL != arg[i]; i++ ) {	
		s += strlen(strcpy(&f[s], arg[i]));
/*		f[s++] = '\xff';  bothers Ultrix MIPS compiler */
		f[s++] = '\377';  
	}
	f[s] = '\0';
	return f;
}
