#define I_SYS
#define I_SOCKET
#define I_ERRNO
#define I_IOCTL
#define I_STRING

#include "includes.h"

#include "debug.h"
#include <sys/stat.h>
#include <arpa/inet.h>

#ifndef S_ISREG
#define S_ISREG(a) (((a) & S_IFMT) == S_IFREG)
#define S_ISDIR(a) (((a) & S_IFMT) == S_IFDIR)
#endif

#ifdef X_DEBUG
static int x_debug = 0;
#endif

extern do_stats(un_char *, int, struct Client *);

/*
 * This modules handles multiplexing the clients onto the serial stream.
 * 
 * do_link_in() is called when there are in packets waiting, and
 * do_link_out() is called when there is something in the link_out buffer and
 * 	the serial out buffer is empty.
 */

/*-----------------------------------------------------------------------*/
/* Local function protypes */
int get_data(un_char *, int);
void put_data(un_char *, int);
int get_client_data(struct Client *);
void put_client_data(struct Client *, int);
/*-----------------------------------------------------------------------*/
/* Data */

int curr_in_stream = -1, 
  curr_out_stream = -1 ;
/* we have this in the open to peek to see if compression is 
 * wanted...  croutons
 */
static struct Client *curr_client = 0;
int new_packet = 0;

/*-----------------------------------------------------------------------*/
void do_link_out(void) {
  int len, n;
  /* Add another packet to the out packet list */
  
  if (p_out_num >= window_size) {
				/* naff off. The packet window is full. */
				/* This is actually normal, so don't */
				/* print a message. */
#if 0
    DEBUG_LINK(stderr, "Tried to do link_out with p_out_num == %d\n",
	       p_out_num);
#endif
    return;
  }
  /* put some data in the packet. Get up to 'max' bytes. */
  /* returns the length. If the length is -ve then it has been compressed */
  if (clients_waiting < 1)
    n = 1;
  else if (clients_waiting < 8)
    n = clients_waiting;
  else n = 8;
  
  new_packet = 1;

  len = get_data(p_out[p_out_s].data, ((out_mask - 12) / n) + 10);
  if (!len) {
    /* All the data waiting was control data for local daemon */
    /* We can handle this. */
    return;
  }
  p_out[p_out_s].timeout = 0; /* Transmit it right away.. */
  p_out[p_out_s].trans = 0;
  p_out[p_out_s].len = len > 0 ? len : -len;
  p_out[p_out_s].type = (len > 0) ? (seven_bit_out ? 2 : 0) : 1;
  DEBUG_LINK(stderr, "%s:Added pack %d to out Q\n", term_server, p_out_s);
  p_out_s = (p_out_s + 1) & 31;
  p_out_num ++;
}


void do_link_in(void) {
  /* Takes packet of the in packet list and feeds it to clients. */
  static un_char uncomp_buff[2049];
  int l;
  while (p_in[p_in_e].type >= 0) {
    /* feed data out */
    DEBUG_LINK(stderr, "%s: Handleing p %d off in Q\n", term_server, p_in_e);
    if (p_in[p_in_e].type == 1) {
      extern int stat_uncomp_in, stat_uncomp_out;
      l =uncompress(p_in[p_in_e].data , p_in[p_in_e].len,
		    uncomp_buff);
      stat_uncomp_in += p_in[p_in_e].len;
      stat_uncomp_out += l;
      put_data(uncomp_buff, l);	
    } else if (p_in[p_in_e].type == 0) {
      put_data(p_in[p_in_e].data, p_in[p_in_e].len);
    } else {
      l = s_2_e_buff(p_in[p_in_e].data, uncomp_buff,
		     p_in[p_in_e].len);
      put_data(uncomp_buff, l);
    }

    p_in[p_in_e].type = -1;
    p_in_e = (p_in_e + 1) & 31;
    p_in_num --;
  }
}

/*---------------------------------------------------------------------------*/

/* This is where compression will eventually get done */
/* For now , we just get some bytes */
/* we are compressing now. */
int get_data(un_char *b, int len) {
  extern int tok_byte_width_out;
  int i, j, k;
  /* this is a horrible kludge, but without major reorignization, 
   * this is the simplest way.  we need to know if the current 
   * packet for the current client should be compressed, so we 
   * need to know the client.  we get the first byte (which forces
   * which client we are getting data from) and then peek and 
   * see what that client wants. we then pass along the byte
   * that we read.. (this is the REAL kludge).
   * croutons.
   */
				/* Get a byte, so we we have a client. */
  k = get_client_byte();
  if ( k < 0 )			/* no data */
  	return 0;
				/* Now that we have a client, we can */
				/* check to see wether we want to */
				/* compress the data or not. */
  if (curr_client->compress && (j = compress(b, len-1, k)) > 0)  {
    j = ( j + tok_byte_width_out - 1) / tok_byte_width_out;
    return -j;
				/* If we only have a seven bit output */
				/* line, then we want to pack 8 bytes */
				/* to 7 seven bit bytes. */
  } else if (seven_bit_out) {
    len = (len * 7) / 8;
    b[0] = 0;
    j = e_2_s_put(b, k, 0);
    while ((j>>3) < len) {
      if ((i = get_client_byte()) < 0) 
	break;
      j = e_2_s_put(b, i, j);
    }
    return (j>>3) + 1;
  } else {			/* Else, just dump the data, we can */
				/* handle it. */
    b[0] = k; /* put the first byte in the array */
    j = 1;
  
    while (j < len) {
      i = get_client_byte();
      if (i < 0) break;
      b[j ++ ]  = i;
    }
  }
  return j;
}
/*---------------------------------------------------------------------------*/
#define ADD_BUFF(clt, c) \
  ((clt)->out_buff.data[(clt)->out_buff.start++] = (c), \
   (((clt)->out_buff.start == 2048) ? ((clt)->out_buff.start = 0) : 0), \
   (++(clt)->out_buff.size))
/*
#undef ADD_BUFF
void ADD_BUFF(struct Client *clt, int c)
{
  ((clt)->out_buff.data[(clt)->out_buff.start++] = (c), \
   (((clt)->out_buff.start == 2048) ? ((clt)->out_buff.start = 0) : 0), \
   (++(clt)->out_buff.size)) ;
  if ((clt)->out_buff.size >= BUFFER_SIZE - 2)
    fprintf (stderr, "ADD_BUFF buffer overflow %d\n", (clt)->out_buff.size);
}
*/

#define ADD_IN_BUFF(clt, c) \
  ((clt)->in_buff.data[(clt)->in_buff.start++] = (c), \
   (((clt)->in_buff.start == 2048) ? ((clt)->in_buff.start = 0) : 0), \
   (((clt)->in_buff.size++ ? 0 : ++clients_waiting)))

void clear_buffers(struct Client *cl) {
  cl->in_buff.size = cl->in_buff.start = cl->in_buff.end = 0;
  cl->out_buff.size = cl->out_buff.start = cl->out_buff.end = 0;
}

void add_ret_buff(struct Client *cl, int which, int byte)  {
  if (!which)
    ADD_IN_BUFF(cl, byte);
  else
    put_client_data(cl, byte);
}

void add_ret_buff_str(struct Client *cl, int which, char *s) {
  int i;
  for (i = 0; s[i];++i)
    add_ret_buff(cl, which, s[i]);
}

void ret_fail(struct Client *cl, int which, int fatal, char *p) {
  add_ret_buff(cl, which, SWITCH);
  add_ret_buff(cl, which, SWITCH-3);
  add_ret_buff(cl, which, I_FAIL);
  if (p) {
    add_ret_buff_str(cl, which, p);
    add_ret_buff_str(cl, which, ": ");
  }
  add_ret_buff_str(cl, which, strerror(errno));
  add_ret_buff(cl, which, 0);
  if(!fatal) return;

  add_ret_buff(cl, which, SWITCH);
  add_ret_buff(cl, which, SWITCH-2);
  add_ret_buff(cl, which, C_CLOSE);
  add_ret_buff(cl, which, 0);
}

void ret_ok(struct Client *cl, int which) {
  add_ret_buff(cl, which, SWITCH);
  add_ret_buff(cl, which, SWITCH-3);
  add_ret_buff(cl, which, I_OK);
  add_ret_buff(cl, which, 0);
}

void do_control(int local , struct Client *cl, un_char *c) {
  int i;
#ifdef SYSV
  struct utsname unam;
#endif
  DEBUG_FP(stderr, "%s:do_control on client %d:%s:\n", term_server,
	   cl->number, c);
  switch(c[0]) {
  case C_NAME:
    DEBUG_FP(stderr, "%s:C_NAME\n", term_server);
    sprintf(cl->name, "%s", c+1);
    ret_ok(cl, local);
    break;
  case C_CLOSE:
    DEBUG_FP(stderr, "%s:C_CLOSE\n", term_server);
    if (cl->state == 1)
      cl->state = 3;		/* Go to flush buffers and close. */
    break;
  case C_CLCLOSE:
    DEBUG_FP(stderr, "%s:C_CLCLOSE\n", term_server);
    if (cl->state == 1)
      cl->state = 4;
    ret_ok(cl, local);
    break;
  case C_DUMB:
    DEBUG_FP(stderr, "%s:C_DUMB\n", term_server);
    cl->type &= ~T_SMART;
    cl->dump_count = 0;
    break;
  case C_DUMP:
    DEBUG_FP(stderr, "%s: C_DUMP %d\n", term_server, atoi((char *) (c+1)));
    ret_ok(cl, local);
    cl->type &= ~T_SMART;
    cl->dump_count = atoi((char *) (c+1))+1;
    break;
  case C_OPEN:
    if (cl->fd >=0)
      close(cl->fd);
    DEBUG_FP(stderr,"%s:got C_OPEN\n", term_server);
    cl->fd = open((char *) (c + 1), O_RDWR | O_CREAT, 0600);
    DEBUG_FP(stderr, "%s:name was %s\n", term_server, c + 1);
    if (cl->fd < 0) {
      DEBUG_FP(stderr, "%s: open failed\n",term_server);
      ret_fail(cl,local,1, "open() failed");
      break;
    }
    cl->type= T_WRFILE;
    cl->cl_type = CL_FILE;
    cl->state = 1;
    ret_ok(cl,local);
    break;
  case C_UPLOAD:
    if (cl->fd>=0) close(cl->fd);
    DEBUG_FP(stderr,"%s:got C_UPLOAD\n", term_server);
    cl->fd = open((char *) (c + 1), O_WRONLY | O_CREAT | O_TRUNC,
			      0600 );
    DEBUG_FP(stderr, "%s:name was %s\n", term_server, c + 1);
    if (cl->fd < 0) {
      DEBUG_FP(stderr, "%s: open failed\n",term_server);
      ret_fail(cl, local, 1, "open() failed");
      break;
    }
    cl->type = T_WRFILE;
    cl->cl_type = CL_FILE;
    cl->state = 1;
    ret_ok(cl, local);
    break;
    
  case C_DOWNLOAD:
    if (cl->fd>=0) close(cl->fd);
    DEBUG_FP(stderr,"%s:got C_DOWNLOAD\n", term_server);
    cl->fd = open((char *) (c + 1), O_RDONLY );
    DEBUG_FP(stderr, "%s:name was %s\n", term_server, c + 1);
    if (cl->fd < 0) {
      DEBUG_FP(stderr, "%s: open failed\n",term_server);
      ret_fail(cl,local, 1, "open() failed");
      break;
    }
    cl->type = T_RDFILE;
    cl->cl_type = CL_FILE;
    cl->state = 1;
    ret_ok(cl,local);
    break;
   
  case C_PTYEXEC:
  case C_EXEC:
    if (cl->fd>=0) close(cl->fd);
    DEBUG_FP(stderr, "%s: %s on client %d (%s) \n", term_server,
	     c[0]==C_PTYEXEC?"C_PTYEXEC":"C_EXEC", cl->number, c+1); 
    if (c[0] == C_PTYEXEC)   cl->fd = open_pty((char *)(c + 1));
    else cl->fd = open_socket((char *)(c + 1));
    if (cl->fd < 0) {
      char *p;
      DEBUG_FP(stderr, "%s: failed to open client: error: %d\n",
	       term_server, cl->fd); 

      switch (cl->fd) {
      case -1: p = "Couldn't get pty"; break;
      case -2: p = "fchmod() failed"; break;
      case -3: p = "fork() failed"; break;
      case -4: p = "socketpair() failed"; break;
      default: p = "Unknown failure"; break;
      }
      ret_fail(cl, local, 1, p);
      break;
    }
    DEBUG_FP(stderr, "%s: opened client\n", term_server);
    cl->type = T_WRFILE | T_RDFILE;
    cl->cl_type = CL_CHILD;
    cl->state = 1;
    cl->pid = pty_pid;
    DEBUG_FP(stderr, "%s: Got pid %d\n", term_server, pty_pid);
    ret_ok(cl, local);
    break;

  case C_BIND:
    DEBUG_FP(stderr, "%s: C_BIND %s\n", term_server, c +1);
    {
      int port;
      int s;
      port = atoi((char *) (c+1));
      s = bind_tcp(port);
      if (s < 0) {
	ret_fail(cl, local , 1, "bind_tcp() failed");
	DEBUG_FP(stderr, "%s:Bind_tcp failed (%d)\n", term_server,
		 port);
      }
      set_nonblock(s);
      if (cl->fd>=0) close(cl->fd);
      cl->fd = s;
      cl->cl_type = CL_BOUND;
      cl->type = T_RDFILE | T_WRFILE;
      cl->state = 1;
      ret_ok(cl, local);
    }
    break;
  case C_ACCEPT:
    {
      struct sockaddr_un dummy;
      int din = sizeof(dummy);
      int s;

      cl->type |= T_RDFILE;

      DEBUG_FP(stderr, "%s: C_ACCEPT %s\n", term_server, c+1);
				/* Get the socket to try and accept() */
				/* on. */
      s = atoi((char *) (c+1));
				/* Error checking..  */
      if (s <  0 || s >= MAX_CLIENTS || clients[s].fd < 0 ||
	  clients[s].cl_type != CL_BOUND) {
	errno = 0;
	ret_fail(cl, local, 1, "Client out of range");
	break;
      }
				/* try the actual accept(). */
      s = accept(clients[s].fd , (struct sockaddr *) &dummy, &din);
      if (s < 0) {
	ret_fail(cl, local, 1, "Accept failed");
	break;
      }

      set_nonblock(s);
      DEBUG_FP(stderr,"%s:got C_ACCEPT\n", term_server);
      if (cl->fd>=0) close(cl->fd);
      cl->fd = s;
      cl->cl_type = CL_SOCKET;
      DEBUG_FP(stderr, "%s:name is %s\n", term_server, c + 1);
      cl->type = T_RDFILE | T_WRFILE;
      cl->state = 1;
      ret_ok(cl, local);
    }
    break;
  case C_SOCKET:
    DEBUG_FP(stderr, "%s: C_SOCKET %s\n", term_server, c+1);
    {

      int s;
      
      s = open_unix((char *)(c+1));
      if (s < 0) {
	DEBUG_FP(stderr, "%s:Open_unix failed (%s)\n", term_server,
		 c+1);
	ret_fail(cl, local, 1, "open_unix() failed");
	break;
      }

      set_nonblock(s);
      DEBUG_FP(stderr,"%s:got C_SOCKET\n", term_server);
      if (cl->fd>=0) close(cl->fd);
      cl->fd = s;
      cl->cl_type = CL_SOCKET;
      DEBUG_FP(stderr, "%s:name is %s\n", term_server, c + 1);
      cl->type = T_RDFILE | T_WRFILE;
      cl->state = 1;
      ret_ok(cl, local);
    }
    break;
  case C_PORT:
    DEBUG_FP(stderr, "%s: C_PORT %s\n", term_server, c+1);
    {
      struct sockaddr_in addr;
      struct hostent *hp, *gethostbyname(), hs;
      char hostname[258];
      int s;
      char *colon, *portname;
      
      if ((s = socket(AF_INET, SOCK_STREAM, 0 )) < 0) {
	ret_fail(cl, local, 1, "Socket() failed");
	break;
      }

      strcpy(hostname, (char *) (c+1));
      colon = strchr(hostname, ':');
      if (colon) {
	*colon = '\0';
	portname = colon + 1;
      }
      else {
#ifdef SYSV
	uname(&unam);
	strcpy(hostname, unam.nodename);
#else
	gethostname (hostname, sizeof(hostname));
#endif
	portname = (char *)(c+1);
      }
      if (hostname[0] >= '0' && hostname[0] <= '9') {
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = inet_addr(hostname);
      }
      else {
	hp=gethostbyname(hostname);
	if (!hp) {
	  ret_fail(cl, local, 1, "gethostbyname() failed");
	  perror ("gethostbyname");
	  close(s);
	  break;
	}
	hs = *hp;
	addr.sin_family = hs.h_addrtype;
	addr.sin_addr = * ((struct in_addr *) hs.h_addr);
      }
      
      addr.sin_port = htons(atoi(portname));
      
      DEBUG_FP(stderr, "Connecting to host %s port %d\n", hostname, 
	atoi(portname) );

      if (connect(s,(struct sockaddr *)&addr,sizeof(struct
						    sockaddr_in))<0) {
	ret_fail(cl,local, 1, "connect() failed");
	perror ("connect");
	close(s);
	break;
      }
      
      set_nonblock(s);
      DEBUG_FP(stderr,"%s:got C_PORT\n", term_server);
      if (cl->fd>=0) close(cl->fd);
      cl->fd = s;
      cl->cl_type = CL_SOCKET;
      cl->type = T_RDFILE | T_WRFILE;
      cl->state = 1;
      ret_ok(cl, local);
    }
    break;
  case C_PRIORITY:
    DEBUG_FP(stderr, "%s: C_PRIORITY %d\n", term_server, atoi((char *) (c+1)));
    cl->priority = atoi((char *) (c+1));
    ret_ok(cl, local);
    break;
  case C_COMPRESS:
    DEBUG_FP(stderr, "%s: C_COMPRESS %c\n", term_server, c[1]);
    
    switch(c[1]) {
    case 'y':	/* yes */
    case 'c':	/* compress */
    case 'Y':	/* caps too */
    case 'C':   
    case 1: 	/* true */
    case '1':	/* in ascii */
      cl->compress = 1;
      ret_ok(cl, local);
      break;
    case 'n':	/* no */
    case 'u':	/* uncompress */
    case 'r':	/* raw */
    case 'N':	/* caps too */
    case 'U':	
    case 'R':
    case 0:  	/* false */
    case '0':  	/* in ascii */
      cl->compress = 0;
      ret_ok(cl, local);
      break;
    default:
      ret_fail(cl, local, 0, "Invalid argument");
      break;
    }
    break;
  case C_STAT:
    {
      struct stat st;
      int type, permissions;
      char buff[10];
      DEBUG_FP(stderr, "%s: C_STAT %s\n", term_server, c+1);
      if (stat((char *) (c+1), &st)< 0) {
	ret_fail(cl, local, 0, "stat() failed");
	DEBUG_FP(stderr, "%s: stat() failed\n", term_server);
	break;
      }
      add_ret_buff(cl, local,SWITCH);
      add_ret_buff(cl, local,SWITCH-3);
      add_ret_buff(cl, local,I_OK);
				/* Get type */
      if (S_ISREG(st.st_mode)) type = 0;
      else if (S_ISDIR(st.st_mode)) type = 1;
      else type = 2;
				/* Now get permissions. */
      permissions = st.st_mode;
      if (getuid() == st.st_uid)
	permissions >>=6;
      else if (getgid() == st.st_gid)
	permissions >>=3;
      permissions &= 07;
      
      sprintf(buff, "%d %d %d", 
	      st.st_size, type, permissions);
      add_ret_buff_str(cl, local, buff);
      add_ret_buff(cl, local,0);
      break;
    }
  case C_STATS: 
    {
      int opt;
      un_char ret[2000];
/*      extern do_stats(un_char *, int, struct Client *); */

      DEBUG_FP(stderr, "%s:C_STATS\n", term_server);
      opt = atoi((char *) (c+1));
      add_ret_buff(cl, local, SWITCH);
      add_ret_buff(cl, local, SWITCH-3);
      add_ret_buff(cl, local, I_OK);

      do_stats(ret, opt, cl);

      add_ret_buff_str(cl, local, (char *)ret);
      add_ret_buff(cl, local, 0);
      break;
    }
  case C_SEEK:
    DEBUG_FP(stderr, "%s:C_SEEK %s\n", term_server, c+1);
    i = atoi((char *) (c+1));
    if (lseek(cl->fd, i, 0)< 0) {
      ret_fail(cl, local, 0, "lseek() failed");
      DEBUG_FP(stderr, "C_seek failed\n");
      break;
    }
    ret_ok(cl, local);
    break;
#ifdef USE_SIGWINCH
  case C_RESIZE:
    DEBUG_FP(stderr, "%s:C_RESIZE %s\n", term_server, c+1);
    {
      void do_resize(int number, int rows, int cols, int ypixels, int xpixels);
      int number;
      int rows, cols, ypixels, xpixels;
      sscanf((char *) (c+1), "%d %d %d %d %d",
				&number, &rows, &cols, &ypixels, &xpixels);
      do_resize(number, rows, cols, ypixels, xpixels);
      ret_ok(cl, local);
    }
    break;
#endif	/* USE_SIGWINCH */
  default:
    break;
  } /* switch */
} /* function */

void init_client(struct Client *cl) {
  extern int compressing; /* the default from main */
  cl->type = T_RDFILE | T_WRFILE;
  cl->dump_count = 0;
  cl->cl_type = CL_SOCKET;
  cl->state = 1;
  cl->compress = compressing;
  cl->c_state = 0;
  cl->number = cl - &clients[0];
  cl->priority = 2;		/* default priority. Higher is better. */
  cl->name[0] = 0;
  clear_buffers(cl);
  DEBUG_LINK(stderr, "Init client %d\n", cl->number);
}

/*---------------------------------------------------------------------------*/
/* Returns next client to read. Will be beefed up later to support priorities*/
/* A client with a priority of n will get n/(sum) of the packets available   */
/* maybe :) */

struct Client * get_next_client(void) { 
  int i, h = -10000, j=0;
  static int c = 0;
				/* Check to see if any of the clients */
				/* are closeing. This gets priority. */
  for (i = 0; i < MAX_CLIENTS;++i)
    if (clients[i].state == 2) {
      DEBUG_STATE(stderr, "get_n_c ret cl %d\n", i);
      return &clients[i];
    }
				/* Then run down the clients looking */
				/* for the next ready client with the */
				/* highest priority. */
  for (i = 0; i < MAX_CLIENTS;++i, c = (c+1) % MAX_CLIENTS)
    if (clients[c].in_buff.size) {
      if (clients[c].priority > h) {
	h = clients[c].priority;
	j = c;
      }
    }

  if (h == -10000) return 0;	/* No client was ready. */

  c = (j+1) % MAX_CLIENTS;	/* Start searching at next client next */
				/* time. */
  return &clients[j];
}

int get_client_data(struct Client *cl) {
  int i;			/* If nothing ready, signal that. */
  SANITY(cl);
  if (!cl->in_buff.size) 
    return -1;
  SANITY(cl->in_buff.end < 2048);
  SANITY(cl->in_buff.end >= 0);
  SANITY(cl->in_buff.size >=0);
  SANITY(cl->in_buff.size < 2048);

				/* get the next byte from the buffer. */
  i = cl->in_buff.data[cl->in_buff.end++];
  if (cl->in_buff.end == 2048)	/* Wrap the buffer round if we have */
				/* hit the end. */
    cl->in_buff.end = 0;
  cl->in_buff.size --;		/* Update count of bytes left in buffer. */
  if (!cl->in_buff.size)	/* If we have emptied it, update the */
				/* number of clients with non-empty */
				/* buffers. */
    --clients_waiting;
  SANITY(clients_waiting >=0 );
  SANITY(cl->dump_count >=0 );
  if (cl->dump_count) {		/* If we are currently dumping, update */
				/* the dump count, and go smart if we */
				/* have finished. */
    if (!--cl->dump_count)
      cl->type |= T_SMART;
  }

  DEBUG_STATE(stderr, "\tgcd:%d\n", i);
  return i;
}

void put_client_data(struct Client *cl, int i) {
  while (1) {
    DEBUG_STATE(stderr, "p_c_d: %d: state %d i %d c_len %d\n", 
		cl->number, cl->c_state, i, cl->c_len);
    switch (cl->c_state) {
    case 0:
      if (i == SWITCH) {
	cl->c_state = 1;
	return;
      }
      
      ADD_BUFF(cl, i);
      return;
    case 1:
      if (i == SWITCH) {
	ADD_BUFF(cl, i);
	cl->c_state = 0;
	return ;
      }
      cl->c_len = 0; /* yes. We do want to throw this byte away */
      /* It is just a remote control message flag */
      if (i == SWITCH - 3) {	/* It is a result message. */
	if (cl->type & T_SMART) {
	  ADD_BUFF(cl, SWITCH);
	  ADD_BUFF(cl, SWITCH-3);
	}
	cl->c_state = 3;
      } else {
	cl->c_state = 2;
      }
      return;
    case 2:
      cl->control[cl->c_len++] = i;
      if (i) return;
      do_control(0, cl , cl->control);
      cl->c_state = 0;
      return;
    case 3:
      if (cl->type & T_SMART) ADD_BUFF(cl, i);
      if (i) return;
      cl->c_state = 0;
      return;
    default:
      cl->c_state = 0;
      break;
    }
  }
}



/* Return the next byte that should go down the serial link */
/* Another bloody finite state machine. ;) */
/* This was a bitch to write. Sigh. More things than I thought needed */
/* to be handled. And it still isn't perfect. :( */

int get_client_byte() {
  static int state = 0,
  next, max;
  
  static char control[255];
  struct Client *cl;
  int i;
  int first = 1;

  while (1) {
    DEBUG_STATE(stderr, "get_c_b: state %d next %d max %d cl %d\n", state,
		next, max, !curr_client ? -1 : curr_client->number);
    first = 0;

    switch(state) {
    case 0: /* looking for new client */
      new_packet = 0;
      cl = get_next_client();
      if (cl == 0) 
	return -1;
      
      if (!cl->in_buff.size && cl->state == 2) { /* closeing down */
				/* We have emptied the buffers, so */
				/* just tell the remote end that , and */
				/* finish off. */
	DEBUG_FP(stderr, "%s:sending C_CLOSE\n", term_server);
	cl->state = -1;
	state = 4;
	sprintf(control, "%c%c%c%c%c%c", SWITCH, cl->number + SWITCH + 1,
		SWITCH, SWITCH - 2, C_CLOSE, 0);
	curr_client = cl;
	next = 0;
	max = 6;
	break;
      }
      
      if (cl != curr_client) {
	curr_client = cl;
	state = 1; 
	return SWITCH;
      }
      state = 2;
      break;    
    case 1:
      state = 2;
      return curr_client->number + SWITCH + 1;
      break;
    case 2:
				/* If we are a new packet, then check */
				/* to see if there is a new client */
				/* with a greater probability. */
      if (new_packet) {
	new_packet = 0;
	state = 0;
	break;
      }

      i = get_client_data(curr_client);
      if (i == SWITCH) {
	if (!(curr_client->type & T_SMART)) {
	  state = 4;
	  max = 1; next = 0;
	  control[0] = SWITCH;	
	  return SWITCH;
	}
	state = 3;
	break;
      } else if (i < 0) {
	state = 0;
	break;
      }
      {
	extern int stat_cooked_out;
	++stat_cooked_out;
      }
      return i;
      break;
    case 3:
      i = get_client_data(curr_client);
      if (i < 0)
	return -1;
      
      if (i == SWITCH) {
	/* It is an escaped escape code */
	state = 4;	
	control[0] = SWITCH;	
	max = 1; next = 0;
	return SWITCH;
      }
      /* ok. We have some sort of control message */
      if ( i  > SWITCH) {
	/* Hmm. It is trying to switch streams on it's own. welllll. ok. */
	/* we'll let it. Note that is can only reliably insert 1 byte at */
	/* a time */
	control[0] = i;
	next  = 0;
	max = 1;
	state = 4;
	return SWITCH;
	break;
      }
      if ( i != SWITCH - 1) {
	/* stuff for remote. Just pass it thru */
	/* might not be SWITCH - 2, but if it isn't, we don't want */
	/* to know. ;) */
	control[0] = i;
	next = 0; max = 1;
	state = 4;
	return SWITCH;
	break;
      }
      /* ok. a real control message for us. */
      state = 5;
      next = 0;
      break;
    case 4:
      if (next + 1 == max) 
	state = 2;
      return control[next++];
      break;
    case 5: /* A local control message */
      /* note that there is a nasty bit here. All other streams block while */
      /* we are waiting for this control message. I thought about */
      /* programming around this but decided that it was too messy. */
   
      /* note that most local control messages don't make much sense. */
      i = get_client_data(curr_client);
      if (i < 0)
	return -1;
      
      control[next++] = i;
				/* If this isn't the end of the */
				/* message, keep going.. */
      if (i)
	break;

      do_control(1, curr_client, (un_char *) control);
      state = 2;
      break;
    } /* switch */
  } /* while */
}

/*----------------------------------------------------------------------*/
/* Transfers the next 'len' bytes from the link, to clients.  Note that */
/* we handle control information here. */
void put_data(un_char *b, int len) {
  static struct Client *curr_client = 0;
  static int state = 0;
  int i, d;
  
  i = 0;
  while (i < len) {
    DEBUG_STATE(stderr, "put_d: s %d, cl %d d %d\n", state, 
		!curr_client ? 0 : curr_client->number, b[i]);
    switch(state) {
    case 0:
      d = b[i++];
      if (d == SWITCH) {
	state = 1;
	break;
      }
      /* ok. Just put data to current client */
      if (!curr_client)
	break;
      put_client_data(curr_client, d);
      break;
    case 1:
      d = b[i++];
      /* checked for escaped escape */
      if (d == SWITCH) {
	if (curr_client) {
	  put_client_data(curr_client, d);
	  put_client_data(curr_client, d);
	}
       	state = 0;
	break;
      }
      /* check for stream switch */
      if (d > SWITCH) {
	curr_client = &clients[d - SWITCH - 1];
	if (curr_client->state < 0) 
	  init_client(curr_client);
	DEBUG_LINK(stderr, "Stream switch to %d\n", d - SWITCH - 1);
	state = 0;
	break;
      }
      if (curr_client) {
	put_client_data(curr_client, SWITCH);
	put_client_data(curr_client, d);
      }
      state = 0;
      break;
    default:
      state = 0;
      break;
    } /* switch */
  } /* while */
} /* function */


