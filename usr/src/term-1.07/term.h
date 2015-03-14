#include "config.h"
#include "client.h"

#if defined(_AIX) || defined(sgi)
#define SANITY(a) ((a) ? (void) 0 : (fprintf(stderr, "fatal: line %d file %s\n", \
 __LINE__, __FILE__), abort(), (void) 1))
#else
#define SANITY(a) ((a) ? 0 : (fprintf(stderr, "fatal: line %d file %s\n", \
 __LINE__, __FILE__), abort(),1)) /* '1' keeps Ultrix MIPS compiler happy */
#endif

/* #define BUFFER_SIZE     2048 Already defined in client.h */
/* global junk */
#define MAX_CLIENTS 	64
#define SWITCH		'@'

#define MAX_TYPE	4 /* There are 4 different types of packets */
/* these are: */
/*	uncompress packet */
/* 	compressed packet */
/*      packed (sevenbit) packet */
/*	ack packet */

/* main.c */
extern int
	current_time,
	remote,
	clients_waiting,
        packet_timeout,
        debug, 
        window_size,
        write_noise,
        byte_shift,
        term_errno,
        seven_bit_in,
        seven_bit_out,
        in_mask,
        out_mask,
        baudrate,
        stat_comp_in,
        stat_comp_out,
        stat_modem_in,
        stat_modem_out;


extern char escapes[], ignores[];

#define T_SMART		1
#define T_LOCAL         2
#define T_RDFILE	4
#define T_WRFILE	8

#define CL_CHILD        1
#define CL_SOCKET       2
#define CL_FILE         3
#define CL_BOUND        4

extern struct Client {
    struct Buffer in_buff,
		out_buff;
    int fd;			/* socket file descriptor. */
    int priority;		/* Precedence. Which clients get */
				/* chosen first. */
    int cl_type;		/* What type of fd are we watching. */
    int type;			/* dumb or smart */
				/* This basically gets down to being */
				/* remote or not, but Clients can */
				/* lower their status. */
    int dump_count;
    int state;			/* one of */
				/* not used */
				/* alive, or closeing down */
    int compress;		/* does this client want compression? */
    int pid;
    int c_state;
    un_char control[100];
    int c_len;
    int number;
    char name[20];
    } clients[];

extern int num_clients;
extern int do_shutdown;
/* serial.c */
struct Packet_out {
    un_char data[260];
    int type;		/* flag for compressed or not */
    int len;
    int timeout;	/* What time it will timeout */
    int trans;
    };

struct Packet_in {
    un_char data[260];
    int type;
    int len;
    };

extern struct Packet_out p_out[32];
extern int p_out_s, p_out_e;
extern int p_out_num;

extern struct Packet_in p_in[32];
extern int p_in_e;
extern int p_in_num;

extern struct Buffer serial_out,
     serial_in;

extern void do_serial_in(void);
extern void do_serial_out(int );
extern void serial_init(void);
extern int get_client_byte(void);
extern char *term_server;

#if 1
#define noise(a) do_noise(a)
#define alert(b) do_alert(b)
#else
#define noise(a)
#define alert(a)
#endif

/* misc.c */
extern void update_time(void);
extern void do_debug(int lev, char *c);

extern void set_nonblock(int);
extern void set_block(int);
#define log(a, b) debug(a, b)

extern void do_noise(int);
extern void do_alert(char *);

extern char ** rebuild_arg(char *);

/* pty.c */
extern void pty_init(void);
extern void setup_term(int);
extern int open_pty(char *);
extern int open_socket(char *);
extern int pty_pid;
/* compress.c */
int compress(un_char *, int, int);
int uncompress(un_char *data , int len , un_char *outpath);
int compress_init(void);

int write_from_buff(int, struct Buffer *, int size);
int read_into_buff(int, struct Buffer *, int size);
int add_to_buffer(struct Buffer *, un_char);
int get_from_buffer(struct Buffer *);

/* system.c */
int open_system(char *);

/* checksum.c */
unsigned short check_sum(un_char *d, int len, int mask);
unsigned short update_crc(unsigned short old, unsigned char c);

/* meta.c */
int meta_state(int);

/* sevenbit.c */
int e_2_s_put(un_char *out, un_char data, int key);
int s_2_e_buff(un_char *data, un_char *out, int len);
