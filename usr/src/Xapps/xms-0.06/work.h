/* work.h - public definitions for work distribution module */
/* Copyright (C) 1991 Andreas Gustafsson */

/* **************** types **************** */

typedef struct wf_state wf_state;


/* **************** public functions **************** */

/* wf=wf_init(timeout): initialize work distribution system.  "timeout" 
  should be well larger than the time to complete a single work packet 
  (in milliseconds) */
struct wf_state *wf_init();

/* wf_dispatch_chunk(wf, client, client_data, client_datalen, 
     slave_data, slave_datalen): give a piece of work to the workforce */
void wf_dispatch_chunk();

/* wf_restart(wf): make sure the computation servers are at work */

/* wf_handle_socket_input(wf, client_data): call this when input available */
void wf_handle_socket_input();

/* wf_timed_out(client_data): call when timeout has occurred */
void wf_timed_out();

/* wf_client_died(): call when client no longer wants wf_draw() calls
   despite outstanding requests */
void wf_client_died();

/* wf_print_stats(wf): print statistics about server performance */
void wf_print_stats();

/* wf_socket(wf): get the fd of the slave communications socket
  (for select) */
int wf_socket();



/* **** external functions called from work.c, user must define **** */

/* misc. services */
char *wf_alloc(); 	/* malloc() or equivalent */
char *wf_realloc();	/* realloc() or equivalent */
void wf_free();		/* free() or equivalent */
void wf_error();	/* print msg and exit */
void wf_warn();		/* print msg */

/* wf_add_timeout(millisecs, client_data): cause a call to
   wf_timed_out(client_data) after millisecs have passed; return id */
char *wf_add_timeout();

/* wf_remove_timeout(id): cancel timeout identified by id */
void wf_remove_timeout();

/* wf_draw(client, client_data, reply_data): handle incoming data */
void wf_draw();
