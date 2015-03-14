/*
 * dip		A program for handling dialup IP connecions.
 *		Script language processor.
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include "dip.h"


static _PROTOTYPE( int	do_dial, (int, char **)				);
static _PROTOTYPE( int	do_goto, (int, char **)				);
static _PROTOTYPE( int	do_hostname, (int, char **)			);
static _PROTOTYPE( int	do_help, (int, char **)				);
static _PROTOTYPE( int	do_if, (int, char **)				);
static _PROTOTYPE( int	do_mode, (int, char **)				);
static _PROTOTYPE( int	do_port, (int, char **)				);
static _PROTOTYPE( int	do_print, (int, char **)			);
static _PROTOTYPE( int	do_reset, (int, char **)			);
static _PROTOTYPE( int	do_send, (int, char **)				);
static _PROTOTYPE( int	do_sleep, (int, char **)			);
static _PROTOTYPE( int	do_speed, (int, char **)			);
static _PROTOTYPE( int	do_xterm, (int, char **)				);
static _PROTOTYPE( int	do_wait, (int, char **)				);


static struct {
  char	*name;
  int	(*func)(int, char **);
} commands[] = {
  { "dial",	do_dial		},
  { "goto",	do_goto		},
  { "hostname",	do_hostname	},
  { "help",	do_help		},
  { "if",	do_if		},
  { "mode",	do_mode		},
  { "print",	do_print	},
  { "port",	do_port		},
  { "reset",	do_reset	},
  { "send",	do_send		},
  { "sleep",	do_sleep	},
  { "speed",	do_speed	},
  { "term",	do_xterm	},
  { "wait",	do_wait		},
  (char *)NULL
};
static int timeout;			/* script "wait" timed out	*/
static int errlevel;			/* script command return code	*/
static FILE *scriptfp = (FILE *)NULL;	/* input script file pointer	*/


static _PROTOTYPE( void TimeOut, (int sig)				);
static _PROTOTYPE( char cvt_char, (char c)				);
static _PROTOTYPE( int	getargs, (char *string, char *arguments[])	);


static void TimeOut(sig)
int sig;
{
  (void) sig;
  timeout = 1;
}


/* Convert a C-style backslash sequence to ASCII. */
static char cvt_char(c)
char c;
{
  if (c == '\0') return(c);
  switch(c) {
	case 'a':
		return('\a');
		break;
	case 'b':
		return('\b');
		break;
	case 'f':
		return('\f');
		break;
	case 'n':
		return('\n');
		break;
	case 'r':
		return('\r');
		break;
	case 't':
		return('\t');
		break;
	case 'v':
		return('\v');
		break;
	case '\\':
		return('\\');
		break;
	case '\'':
		return('\'');
		break;
	case '"':
		return('\"');
		break;
	case '?':
		return('\?');
		break;
	default:
		return('?');
  }
  /* NOTREACHED */
  return('?');
}


/* Split the input string into multiple fields. */
static int getargs(string, arguments)
char *string;
char *arguments[];
{
  register int i;
  register char *sp;
  int argc;

  sp = string; i = 0;
  arguments[i] = sp;
  while (*sp && i < 32) {
        while (*sp && (*sp == ' ' || *sp == '\t')) sp++;
        arguments[i++] = sp;
        while (*sp && *sp != ' ' && *sp != '\t') sp++;
        if (*sp != '\0') *sp++ = '\0';
  }
  argc = i;
  while (i < 32) arguments[i++] = (char *)NULL;
  return(argc);
}


static int do_dial(argc, argv)
int argc;
char *argv[];
{
  if (argc != 2) {
	fprintf(stderr, "Usage: dial telno\n");
	return(-1);
  }

  if (var_modem[0] == '\0') {
	fprintf(stderr, "Please set MODEM first.\n");
	return(-1);
  }
  return(mdm_dial(argv[1]));
}


/* Go to some label in the script. */
static int do_goto(argc, argv)
int argc;
char *argv[];
{
  char buff[128];
  off_t oldpos;
  char *label, *xp;
  register char *sp;

  if (argc != 2) {
	fprintf(stderr, "Usage: goto label\n");
	return(-1);
  }
  if (scriptfp == stdin) {
	fprintf(stderr, "dip: GOTO not possible in TEST mode!\n");
	return(-1);
  }

  label = argv[1];
  oldpos = ftell(scriptfp);
  rewind(scriptfp);
  (void) fflush(scriptfp);

  do {
        if (fgets(buff, 128, scriptfp) == (char *)NULL) break;
	if ((sp = strchr(buff, '\n')) != (char *)NULL) *sp = '\0';
	sp = buff;
	while (*sp == ' ' || *sp == '\t') sp++;
	if (*sp == '#' || *sp =='\0') continue;
	if ((xp = strchr(sp, ':')) == (char *)NULL) continue;
	*xp = '\0';
	if (!strcmp(label, sp)) {
		oldpos = ftell(scriptfp);
		(void) fseek(scriptfp, oldpos, SEEK_SET);
		return(0);
	}
  } while(1);
  (void) fseek(scriptfp, oldpos, SEEK_SET);
  (void) fflush(scriptfp);
  return(-1);
}


/* Show some help. */
static int do_help(argc, argv)
int argc;
char *argv[];
{
  register int i, j;

  i = 0; j = 0;
  printf("DIP knows about the following commands:\n\n");
  while (commands[i].name != (char *)NULL) {
	if (j++ == 0) printf("\t");
	printf("%-8.8s ", commands[i].name);
	if (j == 5) {
		printf("\n");
		j = 0;
	}
	i++;
  }
  if (j != 0) printf("\n\n");
    else printf("\n");
  return(0);
}


/* Set the SLIP hostname of the other side. */
static int do_hostname(argc, argv)
int argc;
char *argv[];
{
  struct hostent *hp;

  if (argc != 2) {
	fprintf(stderr, "Usage: hostname name\n");
	return(-1);
  }
  strncpy(var_host, argv[1], 128);
  if ((hp = gethostbyname(var_host)) == (struct hostent *)NULL) {
	herror(var_host);
	strcpy(var_host, "");
	return(-1);
  }
  strncpy(var_host, hp->h_name, 128);
  memcpy((char *) &var_him, (char *) hp->h_addr_list[0], hp->h_length);
  return(0);
}


/* Check some error (result) code. */
static int do_if(argc, argv)
int argc;
char *argv[];
{
  char *cmd[3];
  char opcode;
  long val, var;
  int ret;

  if (argc != 6) {
	fprintf(stderr, "Usage: if expr goto label\n");
	return(-1);
  };
  if (!strcmp(argv[2], "==")) opcode = '=';
    else if (!strcmp(argv[2], "!=")) opcode = '@';
    else if (!strcmp(argv[2], "<")) opcode = '<';
    else if (!strcmp(argv[2], ">")) opcode = '>';
    else if (!strcmp(argv[2], "<=")) opcode = 'L';
    else if (!strcmp(argv[2], ">=")) opcode = 'G';
    else {
	fprintf(stderr, "Syntax error: \"%s\" is not an opcode!\n", argv[2]);
	return(-1);
  }
  val = (long) atol(argv[3]);

  if (!strcmp(argv[1], "$errlvl")) var = (long) errlevel;
    else if (!strcmp(argv[1], "$ip")) var = (long) var_him.s_addr;
    else {
	fprintf(stderr, "Invalid variable \"%s\" !\n", argv[1]);
	return(-1);
  }

  ret = -1;
  switch(opcode) {
	case '=':	/* EQUAL */
		ret = (var == val);
		break;
	case '@':	/* NOT EQUAL */
		ret = (var != val);
		break;
	case '<':	/* LESS */
		ret = (var < val);
		break;
	case 'L':	/* LESS-EQ */
		ret = (var <= val);
		break;
	case '>':	/* GREATER */
		ret = (var > val);
		break;
	case 'G':	/* GREATER-EQ */
		ret = (var >= val);
		break;
  }

  if (strcmp(argv[4], "goto")) {
	fprintf(stderr, "Warning: keyword not \"goto\" !\n");
	argv[4] = "goto";
  }

  if (ret != 0) {
	cmd[1] = argv[4];
	cmd[1] = argv[5];
	cmd[2] = (char *)NULL;
	return(do_goto(2, cmd));
  }
  return(-1);
}


/* Enter a specific protocol. */
static int do_mode(argc, argv)
int argc;
char *argv[];
{
  register int i;

  if (argc != 2) {
	fprintf(stderr, "Usage: mode protocol_name\n");
	return(-1);
  }

  if (var_him.s_addr == INADDR_ANY) {
	fprintf(stderr, "Please set HOSTNAME first.\n");
	return(-1);
  }

  if ((i = get_prot(argv[1])) == 0) {
	fprintf(stderr, "Usage: mode protocol_name\n");
	return(-1);
  }
  var_prot = i;
  i = (*protosw[var_prot - 1].func)(-1, var_him, var_mtu);
  return(i);
}


/* Set the name of the terminal port to use. */
static int do_port(argc, argv)
int argc;
char *argv[];
{
  if (argc != 2) {
	fprintf(stderr, "Usage: port tty_name\n");
	return(-1);
  }
  strcpy(var_port, argv[1]);
  if (opt_v == 1) printf("Set PORT to \"%s\"\n", var_port);

  /* Initialize the serial line. */
  if (tty_init(var_port) < 0) return(-1);

  return(0);
}


/* Print the contents of some variable. */
static int do_print(argc, argv)
int argc;
char *argv[];
{
  register int i;
  register char *sp;

  if (argc == 1) {
	printf("\n");
	return(0);
  }
  i = 0;
  while (argv[++i] != (char *)NULL) {
	sp = argv[i];
	if (i != 1) printf(" ");
	if (*sp == '$') {
		if (!strcmp(++sp, "errlvl")) printf("%d", errlevel);
		  else if (!strcmp(sp, "ip")) printf("%s", inet_ntoa(var_him));
		  else if (!strcmp(sp, "host")) printf("%s", var_host);
		  else if (!strcmp(sp, "modem")) printf("%s", var_modem);
		  else if (!strcmp(sp, "port")) printf("%s", var_port);
		  else if (!strcmp(sp, "speed")) printf("%ld", var_speed);
		  else fprintf(stderr, "Print: unknown variable %s\n", sp);
	} else printf("%s", sp);
  }
  printf("\n");
  return(0);
}


/* Reset the modem. */
static int do_reset(argc, argv)
int argc;
char *argv[];
{
  if (argc != 1) {
	fprintf(stderr, "Usage: reset\n");
	return(-1);
  }

  /* Did we get a TTY name to work with? */
  if (var_port[0] == '\0') {
	fprintf(stderr, "Please set PORT first.\n");
	return(-1);
  }

  /* Reset the modem. */
  (void) mdm_reset();
  return(0);
}


/* Send a string to the serial driver. */
static int do_send(argc, argv)
int argc;
char *argv[];
{
  register char *sp;
  register char c;

  if (argc != 2) {
	fprintf(stderr, "Usage: send text\n");
	return(-1);
  }

  sp = argv[1];
  while(*sp != '\0') {
	switch(*sp) {
		case '~':
			tty_putc('\r');
			tty_putc('\n');
			break;
		case '\\':
			sp++;
			c = cvt_char(*sp);
			tty_putc((int) c);
			break;
		default:
			c = *sp;
			tty_putc((int) c);
	}
	sp++;
  }
  tty_putc(-1);		/* flush the connection */
  return(0);
}


/* Wait some time. */
static int do_sleep(argc, argv)
int argc;
char *argv[];
{
  int secs;

  if (argc != 2) {
	fprintf(stderr, "Usage: sleep time_in_secs\n");
	return(-1);
  }

  secs = atoi(argv[1]);
  (void) sleep(secs);
  return(0);
}


/* Set the correct SPEED to use. */
static int do_speed(argc, argv)
int argc;
char *argv[];
{
  int speed;

  if (argc != 2) {
	fprintf(stderr, "Usage: speed baudrate\n");
	return(-1);
  }
  if (var_port[0] == '\0') {
	fprintf(stderr, "Please set PORT first.\n");
	return(-1);
  }
  speed = atoi(argv[1]);
  if (opt_v == 1) printf("Speed set to %d\n", speed);

  return(tty_speed(speed));
}


/* Enter a TERMINAL mode. */
static int do_xterm(argc, argv)
int argc;
char *argv[];
{
  if (argc != 1) {
	fprintf(stderr, "Usage: term\n");
	return(-1);
  }
  if (var_port[0] == '\0') {
	fprintf(stderr, "Please set PORT first.\n");
	return(-1);
  }
  do_term();
  return(0);
}


/* Wait for some string to arrive. */
static int do_wait(argc, argv)
int argc;
char *argv[];
{
  char c, c2, *p;
  int howlong;
  _PROTOTYPE( void (*oldsig), (int) );

  if (argc == 1 || argc > 3) {
	fprintf(stderr, "Usage: wait text [timeout_value]\n");
	return(-1);
  }

  if (argc == 3) howlong = atoi(argv[2]);
    else howlong = 0;
  oldsig = signal(SIGALRM, TimeOut);
  (void) alarm(howlong);

  p = argv[1];
  timeout = 0;
  while(!timeout && *p != '\0') {
	c = (char) tty_getc();
	c &= 0177;
	if (timeout == 1) break;
	if (*p == '\\') c2 = cvt_char(*++p);
	  else c2 = *p;
	if (c2 != c) p = argv[1];
	  else p++;
  }
  (void) alarm(0);
  (void) signal(SIGALRM, oldsig);
  return((timeout == 1) ? -1 : 0);
}


int do_command(fp)
FILE *fp;
{
  char cline[128];
  char *argv[32];
  int argc, i;
  int running;
  register char *sp;

  scriptfp = fp;
  running = 1;
  timeout = 0;
  errlevel = 0;
  do {
	if (scriptfp == stdin) {
		if (opt_v == 1) printf("DIP [%-4d]> ", errlevel);
		  else printf("DIP> ");
		fflush(stdout);
	}
        if (fgets(cline, 128, scriptfp) == (char *)NULL) break;
	if ((sp = strchr(cline, '\n')) != (char *)NULL) *sp = '\0';
	sp = cline;
	while (*sp == ' ' || *sp == '\t') sp++;
	if (*sp == '#' || *sp =='\0') continue;
	if (opt_v) fprintf(stderr, ">> %s\n", sp);
	if ((argc = getargs(sp, argv)) == 0) continue;

	/* If this is a label, skip it. */
	if (strchr(argv[0], ':') != (char *)NULL) continue;

	/* Now, check which command it is. */
	if (strcmp(argv[0], "exit") != 0) {
		i = 0;
		while (commands[i].name != (char *)NULL) {
			if (!strcmp(commands[i].name, argv[0])) break;
			i++;
		}
		if (commands[i].name != (char *)NULL) {
			errlevel = (*commands[i].func)(argc, argv);
		} else printf("? Unknown command (%s).\n", argv[0]);
	} else running = 0;
  } while(running);
  return(errlevel);
}
