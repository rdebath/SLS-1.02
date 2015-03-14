#define I_SYS
#define I_ERRNO
#define I_GETOPT
#define I_STRING
#define I_SOCKET
#define I_IOCTL
#include "includes.h"

#include <sys/wait.h>
#include <sys/stat.h>
#include <signal.h>

#include "debug.h"

/*
 *
 * main file. Calls everything else. ;)
 * 
 * basically consists of select() and then calls things based on the select().
 *
 */
/*------------------------------------------------------------------------*/
/* truely global vars. Used everywhere. */
int current_time = 0;
int do_shutdown	 = 0;
int debug = 0;
int compressing = 1;
/*------------------------------------------------------------------------*/
/* Various global variables. Note that most of these vars are local to    */
/* this file. Where possible (i.e. where it doesn't impinge efficency) 	  */
/* this is enforced */
int remote = 0;

int clients_waiting;
struct Client clients[MAX_CLIENTS];
int num_clients = 0;
int baudrate =   2400;
int bytes_left = 1024; /* bytes availiable to send this second.. */
int fudge_flow = 0; /* should we generate periodic control-Q's */
int byte_shift = 0; /* So we can map less frequently uses section down to */
                    /* 0-32 */
int window_size = 3; /* A convenient number. 9600 baud users should probly */
				/* go to 2. */
int packet_timeout = 70; /* wait 3.5 seconds for a packet to time out */
int do_forceing = 1; /* weather or not to automatically transmit packets */
				/* if the window is full, and the out */
				/* buffer is empty. */
int write_noise = 0; /* whether we should print out all the the serial stuff */
				/* we get that we don't understand.. */

int seven_bit_in = 0;		/* Are we on a line that ignores the */
				/* top bit. */
int in_mask = 255;
int seven_bit_out = 0;
int out_mask = 255;
int breakout_char = '0';

int stat_modem_in = 0,
  stat_modem_out = 0,
  stat_cooked_in = 0,
  stat_cooked_out = 0,
  stat_rare_out = 0;

char escapes[256];
char ignores[256];

int modem_in = 0, modem_out = 1;
/*------------------------------------------------------------------------*/
/* module function prototypes */

void main_loop(int);
void main_init(void);
void do_link_in(void);
void do_link_out(void);
/*------------------------------------------------------------------------*/
/* main. */

void main_init(void) {
  int i;
  for (i = 0; i < MAX_CLIENTS;++i) {
    clients[i].fd = -1;
    clients[i].state = -1;
  }
}
				/* In case of sudden death, do some */
				/* minimal clean up. */
void sig_quit(int dummy) {
  set_block(0);
  set_block(1);
  terminal_restore(0);
  terminal_restore(1);
  exit(0);
}
				/* Drop a core. */
void sig_core(int dummy) {
  set_block(0);
  set_block(1);
  terminal_restore(0);
  terminal_restore(1);
  abort();
}
				/* We ignore this signal. There was */
				/* some problem with it on linux. A */
				/* bit odd, and I can't track it down. */
				/* I suspect it is a bad shell. So we */
				/* just ignore it. */
void sig_ignore(int dummy) {
  signal(SIGALRM, sig_ignore);
  signal(SIGPIPE, sig_ignore);
}

void sig_child(int dummy) {
  int p, i;
#ifdef SVR4
  int stat_loc;

  p = waitpid((pid_t)-1, &stat_loc, WNOHANG);
#else
  p = wait3(0, WNOHANG, 0);
#endif /* SVR4 */
#ifndef SYSV
  signal(SIGCHLD, sig_child);
#endif /* SYSV */

  if (p < 1) return;
  DEBUG_MAIN(stderr, "%s:sig child kicked\n", term_server);
  for (i = 0; i < MAX_CLIENTS;++i) {
    if (clients[i].pid == p && clients[i].state > 0) {
      clients[i].state = 2;
      return;
    }
  }
  /* Hmm. child that we don't know about died!?!?! */
  if (dummy)
    dummy = 0;
}

void read_rc(char *p) {
  FILE *f;
  char *home_p;
  char file[200];
  int i;

  home_p = getenv( "HOME" );
  if (!home_p)
    return;

  if (!p)
    sprintf(file, "%s/.term/termrc", home_p);
  else if (p[0])
    sprintf(file, "%s/.term/termrc.%s", home_p, p);
  else
    return;

  f = fopen(file, "r");
  if (!f) return;
  
  while (!feof(f)) {
    fgets(file, 200, f);
				/* skip blank lines + comments. */
    if (!file[0] || file[0] == '\n' || file[0] == '#')
      continue;			
				/* compression on/off */
    if (!strncmp(file, "compress ", 9)) {
      if (!strncmp(file + 9, "off", 3))
	compressing = 0;
    }
				/* Handle setting the breakout */
				/* character.  */
    else if (!strncmp(file, "breakout ",9 )) {
      breakout_char = atoi(file + 9);
    }
    else if (!strncmp(file, "remote", 6)) {
      remote = 1;
    }
    else if (!strncmp(file, "chdir ", 6)) {
      chdir(file + 6);
    }
				/* any special characters to be escapeing */
    else if (!strncmp(file, "escape ", 7)) {	/* If it is an escape.. */
      char *p;
      int j;
      
      i = atoi(file + 7);	/* get the number following. */
      if (i < 0 || i > 255) {	/* check for sanity. */
	fprintf(stderr, "Invalid escape %d in termrc\n", i);
	continue;
      }

      if ((p = strchr(file+7, '-')) != NULL) { /* See if this is a */
					     /* range.. */
	while (*++p == ' ');	/* skip whitespace. Note that it */
				/* automatically skips the '-'. */
	j = atoi(p);		/* if it is, then get the second number. */
	if (j < 0 || j > 255) {	/* sanity check again. */
	  fprintf(stderr, "Invalid range limit %d in termrc\n", j);
	  continue;
	}
	for (;i != j; i = (i+1) & 255) /* Ok. mark all the characters */
				/* in the range as being escaped. */
	  escapes[i] = 1;   
      }
      else escapes[i] = 1;	/* else just set the one character as */
				/* to be escaped. */
    }
    else if (!strncmp(file, "ignore ", 7)) {	/* If it is an escape.. */
      char *p;
      int j;
      
      i = atoi(file + 7);	/* get the number following. */
      if (i < 0 || i > 255) {	/* check for sanity. */
	fprintf(stderr, "Invalid ignore %d in termrc\n", i);
	continue;
      }

      if ((p = strchr(file+7, '-')) != NULL) { /* See if this is a */
					     /* range.. */
	while (*++p == ' ');	/* skip whitespace. Note that it */
				/* automatically skips the '-'. */
	j = atoi(p);		/* if it is, then get the second number. */
	if (j < 0 || j > 255) {	/* sanity check again. */
	  fprintf(stderr, "Invalid range limit %d in termrc\n", j);
	  continue;
	}
	for (;i != j; i = (i+1) & 255) /* Ok. mark all the characters */
				/* in the range as being escaped. */
	  ignores[i] = 1;   
      }
      else ignores[i] = 1;	/* else just set the one character as */
				/* to be escaped. */
    }
    /* The baudrate */
    else if (!strncmp(file, "baudrate ", 9)) {
      i = atoi(file + 9);
      if (i < 300) i = 300;
      baudrate = i;
    }
    else if (!strncmp(file, "shift ", 6)) {
      byte_shift = atoi(file + 6);
      if (byte_shift < 0 || byte_shift > 255) {
	fprintf(stderr, "Invalid shift %d\n", byte_shift);
	byte_shift = 0;
      }
    }

    else if (!strncmp(file, "window ", 7)) {
      window_size = atoi(file +7 );
      if (window_size < 1 || window_size > 14) {
	fprintf(stderr, "Invalid window size %d. Must be in [1..14]\n", window_size);
	window_size = 3;
      }
    }
    
    else if (!strncmp(file, "timeout ", 8)) {
      packet_timeout = atoi(file + 8);
      if (packet_timeout < 10 || packet_timeout > 24000) {
	fprintf(stderr, "Invalid timeout value (%d). Must be in [10..240]\n", packet_timeout);
	packet_timeout = 50;
      }
    }
    
    else if (!strncmp(file, "force on", 8)) {
      do_forceing = 1;
    }
    else if (!strncmp(file, "noise on", 8)) {
      write_noise = 1;
    } 
    else if (!strncmp(file, "sevenbit", 8)) {
      extern int tok_byte_mask_in, tok_byte_width_in, 
        tok_byte_mask_out, tok_byte_width_out;
      seven_bit_in = 1;
      seven_bit_out = 1;
      in_mask = out_mask = 127;
      tok_byte_mask_in = tok_byte_mask_out = 127;
      tok_byte_width_in = tok_byte_width_out = 7;
    }
    else if (!(strncmp(file, "seven_out", 9))) {
      extern int tok_byte_mask_out, tok_byte_width_out;
      seven_bit_out = 1;
      out_mask = 127;
      tok_byte_mask_out = 127;
      tok_byte_width_out = 7;
    }
    else if (!(strncmp(file, "seven_in", 8))) {
      extern int tok_byte_mask_in, tok_byte_width_in;
      seven_bit_in = 1;
      in_mask = 127;
      tok_byte_mask_in = 127;
      tok_byte_width_in = 7;
    }

    /* Should we generate flow control characters ?? */
    else if (!strncmp(file, "flowcontrol ", 12)) {
      fudge_flow = atoi(file+12);
      if (fudge_flow < 0) fudge_flow = 0;
    } 
    else {
      fprintf(stderr, "Unrecognized line in ~/.term/termrc.\n");
      fprintf(stderr, "'%s'\n", file);
    }
  }
  fclose(f);
}

void main(int argc, char *argv[]) 
{
  extern char *optarg;
  extern int optind, opterr;
  
  int s, c, i;
  char sock_unix[109];
  char *home;
  
  /* initalize character escapeing. */
  for (i = 0; i < 256;i++) {
    ignores[i] = 0;
    escapes[i] = 0;
  }

  escapes['^'] = 1;

  main_init();

  /* read in the termrc file. */
  read_rc(0);

  /* then check env variables. */
  setbuf(stderr, 0);
  if (getenv("BAUDRATE")) {
    baudrate = atoi(getenv("BAUDRATE"));
    if ( baudrate < 300)
      baudrate = 300;
  }
  /* Then check command line options */
  /*
    The code to parse the command line was written by the
    one and only Muhammad Saggaf. If you have any question
    about Linux, networking, Unix, or life in general, 
    don't ask him mate! :). Chances are he doesn't know the 
    answer.
    */
  
  opterr = 0;
  
  while ((c = getopt (argc, argv, "ac:d:f:l:n:ors:t:v:w:")) !=EOF)
    switch(c) {
    case 'a':
      {
	extern int tok_byte_mask_out, tok_byte_width_out,
	  tok_byte_mask_in, tok_byte_width_in;
	tok_byte_mask_out = tok_byte_mask_in = 127;
	tok_byte_width_out = tok_byte_width_in = 7;

	seven_bit_in = seven_bit_out = 1;
	in_mask = out_mask = 127;
	break;
      }
    case 'f': 
      fudge_flow = atoi(optarg);
      if (fudge_flow < 0) fudge_flow = 0;
      break;
    case 'l':
      i = open(optarg, O_RDWR | O_CREAT | O_TRUNC, 0644);
      if (i < 0) {
	perror("open");
	fprintf(stderr, "Unable to open log file %s\n", optarg);
	break;
      }
      if (i != 2) {
	close(2);		/* Just to make sure.. */
	dup2(i, 2);
	close(i);
      }
      break;
    case 'o': do_forceing = 1; break;      
    case 'r': remote = 1; break;
    case 't':
      packet_timeout = atoi(optarg);
      if (packet_timeout < 10 || packet_timeout > 240) {
	fprintf(stderr, "Invalid timeout value (%d).\n", packet_timeout);
	packet_timeout = 50;
      }
      break;
    case 'v':

				/* add a device to the list. */
      i = open(optarg, O_RDWR);
      if (i < 0) {
	perror("open");
	fprintf(stderr,"Unable to open modem device %s\n", optarg);
	break;
      }
      close(modem_in);
      close(modem_out);
      modem_in = i;
      modem_out = i;
      break;
    case 'w':
      window_size = atoi(optarg);
      if (window_size < 1 || window_size > 14) {
	fprintf(stderr, "Invalid window size %d\n", window_size);
	window_size = 3;
      }
      break;
    case 'd': 
      debug = atoi(optarg);
      fprintf(stderr, "Debugging = %x\n", debug);
      break;
    case 'c':
      if (!strcmp(optarg, "off"))
	compressing = 0;
      break;
    case 'n':
      if (!strcmp(optarg, "on"))
	write_noise = 1;
      else write_noise = 0;
      break;
    case 's':
      baudrate = atoi(optarg);
      if (baudrate < 300) {
	fprintf(stderr, "baudrate set too low. Reset to 300\n");
	baudrate = 300;
      }
      break;
    default:
      fprintf(stderr, "unrecognized or incomplete argument '%s'. Exiting\n",  argv[optind-1]);
      exit(-1);
    }
  
  if (optind < argc)
    term_server = argv[optind];
  
  for (s = 0; s < MAX_CLIENTS;++s) {
    clients[s].fd = -1;
  }

  if (!term_server) {
    term_server = "";
  }
				/* Read in a specific termrc. */
  read_rc(term_server);

  home = getenv("TERMDIR");
  if (!home)
    home = getenv("HOME");
  if (!home)
    home = "/tmp";   
  
  sprintf(sock_unix,"%s/.term", home);
  mkdir(sock_unix, 0700);
  strcat(sock_unix, "/socket");
  strcat(sock_unix, term_server);
  
  if ((s = bind_unix(sock_unix)) < 0) {
    exit(1);
  }
  /* init modules */
  
  serial_init();
  compress_init();
  update_time();
  
#ifdef SYSV
  sigset(SIGCHLD, sig_child);
#else
  signal(SIGCHLD, sig_child);
#endif /* SYSV */
  signal(SIGHUP, sig_quit);
  signal(SIGPIPE, sig_ignore);
  signal(SIGINT, sig_quit);
  signal(SIGQUIT, sig_quit);
  signal(SIGIOT, sig_core);
  signal(SIGSEGV, sig_core);
  signal(SIGALRM, sig_ignore);

  terminal_save(0);
  set_nonblock(s);
  set_nonblock(modem_in);
  set_nonblock(modem_out);
  terminal_raw(modem_in);
  terminal_raw(modem_out);
  
  main_loop(s);

  set_block(modem_in);
  set_block(modem_out);
  terminal_restore(0);
  terminal_restore(1);

  set_block(s);
}

/*-----------------------------------------------------------------------*/
void check_client(int cl, int ret) {
  DEBUG_MAIN(stderr, "%s: term_errno == %d\n", term_server, term_errno);
  if (!term_errno) return;
  if (clients[cl].state == 3 ) {
    DEBUG_FP(stderr, "%s:truncating out_buff\n", term_server);
    clients[cl].out_buff.size = 0;
    return;
  }
#if 0
  if (ret < 0)
    perror("client gave this");
#endif
				/* Ok. Close the descriptor. */
  close(clients[cl].fd);
  clients[cl].fd = -1;
				/* And go to state 2. */
  clients[cl].state = 2;
}

int new_client(void) {
  int j;

  for (j = remote; j < MAX_CLIENTS;j+=2)
    if (clients[j].fd < 0 && clients[j].state < 0) break;

  if (j == MAX_CLIENTS) return -1; /* not maximum clients */

  DEBUG_FP(stderr, "%s: new client %d.\n", term_server, j);
  clients[j].in_buff.size = 0;
  clients[j].in_buff.start = 0;
  clients[j].in_buff.end = 0;
  clients[j].out_buff.size = 0;
  clients[j].out_buff.start = 0;
  clients[j].out_buff.end = 0;
  clients[j].type = T_SMART | T_RDFILE | T_WRFILE;
  clients[j].dump_count = 0;
  clients[j].cl_type = CL_SOCKET;
  clients[j].compress = compressing;
  clients[j].state = 1;
  clients[j].c_state = 0;
  clients[j].number = j;
  clients[j].priority = 0;
  clients[j].name[0] = 0;
  return j;
}
/*------------------------------------------------------------------------*/
/* Main loop. Hangs around waiting for things to get ready, and calls to  */
/* approroprite routines. 						  */

void main_loop(int socket) {
  struct timeval timeout;
  fd_set reads, writes, excepts;
  int i, j, empty = 1;
  int max = 0;
  
  while (!do_shutdown) {


/* If the serial out buffer is empty, try and put something in it */
    if (bytes_left && !serial_out.size) {
      do_serial_out(0);
      if (!serial_out.size)
	do_link_out();
      if (!serial_out.size) 
	do_serial_out(0);
    }
    
/* Set up client stuff */
/* We select to read if: */
/*	The input buffer is empty */
/* We select to write if: */
/*      The output buffer is not empty */

    FD_ZERO(&reads);
    FD_ZERO(&writes);
    FD_ZERO(&excepts);
    
    if (p_in_num && empty)
      do_link_in();
    
    for (i = 0; i < MAX_CLIENTS;++i) {
				/* If it's closeing down, and the */
				/* buffers are empty, then kill it. */
      if (clients[i].state >= 3 && !clients[i].in_buff.size &&
	  !clients[i].out_buff.size) {
	DEBUG_FP(stderr, "%s:real close %d %d\n", term_server , i,
		 clients[i].state);
	close(clients[i].fd);
	clients[i].fd = -1;
	if (clients[i].state == 3)
	  clients[i].state = -1; 
	else clients[i].state = 1;
	continue;
      }
				/* If it's a file, we don't need to */
				/* select() on it. */
      if (clients[i].cl_type == CL_FILE) continue;
      if (clients[i].fd < 0) continue; /* If it's not a file, and */
				       /* there is no fd, then no */
				       /* select(). */
      if (!clients[i].in_buff.size && (clients[i].type & T_RDFILE) &&
	  clients[i].state == 1) { 
	FD_SET(clients[i].fd, &reads);
      }
      if (clients[i].out_buff.size && (clients[i].type & T_WRFILE)) {
	FD_SET(clients[i].fd, &writes);
      }
      FD_SET(clients[i].fd, &excepts);
      if (max < clients[i].fd) max = clients[i].fd;
    }
    
/* Select for socket and modem */
/* We select read for socket if we aren't at the maxmimum number */
/* of clients */
    if (num_clients < MAX_CLIENTS) {
      FD_SET(socket, &reads);
      if (max < socket) max = socket;
    }

/* We select for read on the modem if the serial in buffer is empty */
    if (!serial_in.size) {
      FD_SET(modem_in, &reads);
      if (max < modem_in) max = modem_in;
    }

/* Now if there is anything in the serial out buffer, select for writing */

    if (serial_out.size && bytes_left) {
      FD_SET(modem_out, &writes);
      if (max < modem_out) max = modem_out;
    }

/* Set the timeout value for select(). */
    timeout.tv_sec = 0;
    timeout.tv_usec = 500000; /* 0.5 seconds */

/* do select() */	
    if (select(max+1, &reads, &writes, &excepts, &timeout) < 0) {
				/* This is perfectly normal. Things */
				/* that send signals will cause select */
				/* to exit with an error. */
#if 0
      perror("select");
#endif
      continue;
    }	

/* Update current_time. This is maintained in 20th s of a second */
    update_time();

/* start checking to see whats ready and whats not */
/* Can we read from modem  ?? */
    if (!serial_in.size && FD_ISSET(modem_in , &reads)) {
      j = read_into_buff(modem_in, &serial_in, 0);
      if ( j < 0 && errno != ERR_BLOCK) {
	perror("read from modem");
	return;
      }
      else stat_modem_in += j;
    }
    
    if (serial_in.size) 
      do_serial_in();

/* Ok. Can we write to modem ??? */
    if (FD_ISSET(modem_out, &writes) && bytes_left) {
      int t = bytes_left;
      if (t > serial_out.size)
	t = serial_out.size;
      j = write_from_buff(modem_out, &serial_out, t);
      if (j < 1 && term_errno != 1) {
	perror("write to modem");
	return;
      }	  
      else {
	stat_modem_out += j;
	bytes_left -= j;
      }
    }

/* test for new client */
    if (FD_ISSET(socket, &reads)) { /* try for a connect. */
      struct sockaddr_un dummy;
      int din = sizeof(dummy);
      i = accept(socket , (struct sockaddr *) &dummy, &din);
      if (i >= 0) { /* a new client */
	j = new_client();
	clients[j].fd = i;
	clients[j].cl_type = CL_SOCKET;
	set_nonblock(i);
      }
      else if (term_errno != 1) {
#if 0
	perror("accept");
#endif
      }
    }
/* test for data being read from clients */
    for (i = 0; i < MAX_CLIENTS;++i) {
      if (clients[i].fd < 0) continue;
      if (clients[i].in_buff.size) continue;
      switch (clients[i].cl_type) {
      case CL_CHILD:		/* fall through */
      case CL_SOCKET:		/* fall through */
	if (!FD_ISSET(clients[i].fd, &reads) &&
	    !FD_ISSET(clients[i].fd, &excepts))
	  continue;
				/* Fall through. */
      case CL_FILE:		
	if (!(clients[i].type & T_RDFILE)) continue;
	j = read_into_buff(clients[i].fd, &clients[i].in_buff, 0); 
	DEBUG_FP(stderr, "%s:read %d bytes from client %d\n",
		 term_server , j, i);
				/* Did this client start sending?? */
	if (j && j == clients[i].in_buff.size)
	  ++clients_waiting;

				/* Hmmm. Something errored. Lets take */
				/* a look... */
	if (j <= 0)
	  check_client(i, j);
	break;
      case CL_BOUND:
	if (!FD_ISSET(clients[i].fd, &reads)) continue;
	{
	  char num[10]; int k;
				/* We have an accept ready... */
	  clients[i].type &= ~T_RDFILE; /* Don't try reading it untill */
				/* the accept is done. */
	  sprintf(num, "%d", i);
	  for (k =0 ;num[k];++k)
	    add_to_buffer(&clients[i].in_buff, num[k]);
	  add_to_buffer(&clients[i].in_buff, 0);
	}
	break;
      }
    } /* for clients loop */
    
/* test for data being send to clients */
    empty = 1;
    for (i = 0; i < MAX_CLIENTS;++i) {
      if (clients[i].fd < 0) continue;
      if (!clients[i].out_buff.size) continue;
      switch (clients[i].cl_type) {
      case CL_CHILD:
      case CL_SOCKET:
	if  (!FD_ISSET(clients[i].fd, &writes)) continue;
      case CL_FILE:
	if (!(clients[i].type & T_WRFILE)) continue;
				/* something there! :) */
	j = write_from_buff(clients[i].fd, &clients[i].out_buff,0);
	DEBUG_FP(stderr, "%s:write %d bytes to client %d\n", 
		 term_server, j, i);
	if (j <= 0)
	  check_client(i, j);
	else stat_cooked_in += j;
	break;
      case CL_BOUND:
	break;
      }				/* switch */
    }				/* for clients loop */
    
  } /* while loop */
  
} /* function */

