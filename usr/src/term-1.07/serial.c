#include "includes.h"

#include "debug.h"

extern char *term_server;
extern int fudge_flow;
extern int byte_shift;
/*
 * Handles the serial side of things.
 *
 * 4 main routines..
 * do_serial_in() is called when the serial port is ready for reading and
 *	the in packet buffer isn't full.
 * do_serial_out() is called when the serial port is available for writing and
 * 	there are packets waiting.
 *
 * Compression is done at this level. I opted for the computational more 
 * expensive method of trying to compress each packet, rewinding the dictionary
 * if it failed.
 *
 * this module sees 4 buffers.
 * serial_in[]	are characters read from modem.
 * serial_out[] are characters waiting to be sent to modem.
 *
 * Note that serial_in() won't be called if link_in() has any characters in it.
 */
/*----------------------------------------------------------------------*/
/* function prototypes for this module */
void do_ack(int);
void send_ack(int);
int check_match(int, int);

/*----------------------------------------------------------------------*/
/* Various ring buffers */
struct Buffer serial_in = {{0},0,0,0}, serial_out = {{0},0,0,0};
int inhabit_send = 0;

#define PUT_SERIAL(c) add_to_buffer(&serial_out, c)
#define GET_SERIAL() get_from_buffer(&serial_in);

/* Packet information */
struct Packet_out p_out[32];
int p_out_s, p_out_e;
int p_out_num;

struct Packet_in p_in[32];
int p_in_e;
int p_in_num;

void serial_init() {
  int i;
	      
  p_out_s =
  p_out_e = 
  p_out_num =
		    
  p_in_e =
  p_in_num = 0;

  for (i = 0; i < 32;++i) {
    p_out[i].type = -1;
    p_in[i].type = -1;
  }
}

/*---------------------------------------------------------------------------*/
/* Takes a byte, and puts it in the serial buffer. */
/* If nessecery, it emits escape characters untill the byte is valid */
 /* for the serial line. It adds 33. I hope 33 is a generator for */
 /* 0-255. As you can see, it is VERY expensive if there are a lot of */
 /* escaped characters. Hopefully this won't be the case. */

void put_serial(int a) {
  static int f_c = 1;
  if (fudge_flow && !--f_c) {	/* If we need to generate */
				/* flow control characters, and */
				/* there has been enough */
				/* intervening characters, */
				/* then generate an XON. */
    PUT_SERIAL(17);		/* emit an XON */
    f_c = fudge_flow;		/* and reset the counter. */
  }

  a ^= byte_shift;		/* and shift it to try and avoid */
				/* characters that need escapeing. */
  a &= out_mask;

  DEBUG_PED(stderr, "o%X\n", a); /* debugging.. */

  while (escapes[a]) {		/* Ok. While it is a */
				/* character that needs escapeing, we */
				/* emit escapes and try again. */
    PUT_SERIAL('^');		/* emit the escape character. */
    a = (a + 33);		/* and pick a new character. I think */
				/* 33 is a generator for [0..255], but */
				/* I haven't checked. */
    a &= out_mask;
  }
  PUT_SERIAL(a);		/* Now put the character out. */
}

/* collect stats on the distribution of input characters */
void do_histagram(int h)
{
  static int counter = 0;
  static long hist_gram[256];
  h &= 255;
  ++hist_gram[h];
  ++counter;

#if 0
  if (counter > 50000) {
    int i;
    FILE *fp;
    fp = fopen("hist", "w");
    for (i = 0; i < 256;++i)
      fprintf(fp, "%d had %d counts\n", i, hist_gram[i]);
    counter = 0;
    fclose(fp);
  }
#endif

  return;
}
  
  

/* Ok. get a byte from the serial in buffer */
/* we handle character escapes here. */
int get_serial(void) {
  int a;			/* Our byte. */
  static int state = 0;
  
  while (serial_in.size) {
    if (state == 0) {		/* If we aren't in the middle of */
				/* handleing and escape.. */
      a = GET_SERIAL();		/* then get the next byte. */
      a &= in_mask;
      if (a == '^') {		/* Is it an escape?? */
	state++;		/* yes, go to escape handleing. */
	continue;
      }
      if (ignores[a]) continue; /* This char MUST be line noise */
				/* We were told the remote system will */
				/* never generate this character. */
      DEBUG_PED(stderr, "i%X\n", a); /* For debugging.. */
      return (a ^ byte_shift) & in_mask; /* Ok. Return it modulo the */
					 /* byte shift. As the */
					 /* byte_shift could be larger */
					 /* than the mask, mask it */
					 /* again. */
    }

    a = GET_SERIAL();		/* Ok. escape handleing. Get the next */
				/* byte. */
    a &= in_mask;

    if ( a== '^') {		/* Ok! It is an escaped escape. Try again. */
      ++state;
      continue;
    }
    if (ignores[a]) continue;
				/* control junk. else it is line */
				/* noise. Either way, we don't want it. */
    a = (a - 33 * state) ^ byte_shift; /* Ok. Work out what the byte */
				/* should be.. */
    state = 0;			/* and return to normal character */
				/* processing. */
    a &= in_mask;
    DEBUG_PED(stderr, "i%X\n", a); /* debug */
    return a;			/* and return byte to caller. */
  }
  return -1;			/* Nothing left in buffer. Return EOF */
}

/*---------------------------------------------------------------------------*/
/* Main routines. */

/* This is very horrible. We find the packet that has been waiting for a 
 * ack the longest and send it..... This should be a list structure 
 * But that would be very messy to do. (Where do we add new packets?? )
 * This works fine for now. We will wait and see what sort of cpu time 
 * it uses..
 */
void do_serial_out(int force) {
  int i, j, l;
				/* port is ready for writing. see */
				/* whats available. */ 
  
  if (!p_out_num || serial_out.size || inhabit_send)
    return;			/* Hmm. How did this get called. */
  
				/* Check for timeouts.. */
				/* First we find the packet that has */
				/* been waiting longest. */
  l = p_out_s;
  for ( j = p_out_s, i = 0; i < 32;++i, j = ((j+1)&31)) {
    if (p_out[j].type < 0) continue;
    if (p_out[j].timeout < p_out[l].timeout || p_out[l].type < 0)
      l = j;
  }
				/* Then we check to make sure that */
				/* this is longer than the minimum */
				/* packet timeout period and that we */
				/* actually found a packet. */
  if (p_out[l].type < 0)
    return;

  if (current_time - p_out[l].timeout < packet_timeout)
    return;

				/* And then we send it. */
  if (fudge_flow && p_out[l].timeout)
    put_serial(19);

  put_serial('A'+p_out[l].type); 
  put_serial(p_out[l].len);
  put_serial(l);
  j = update_crc(update_crc(update_crc(0, 'A'+p_out[l].type),
			    p_out[l].len), l);
  put_serial(j & 255);
  DEBUG_CHECK(stderr, "%s:header check == %x\n", term_server, j&255);
  
  for (j = 0; j < p_out[l].len;++ j)
    put_serial(p_out[l].data[j]);
  j = check_sum(p_out[l].data, j , out_mask);
  DEBUG_CHECK(stderr, "%s:p %d len %d checksum == %x\n", term_server, 
	      l, p_out[l].len , j);
  put_serial(j & 255);
  put_serial((j >> 8) & 255);
  if (p_out[l].timeout)
    WARNING(stderr, "%s:timed out at %d trans %d\n", term_server, 
	    current_time - p_out[l].timeout, p_out[l].trans);
  p_out[l].timeout = current_time;
  p_out[l].trans++;
}

void do_serial_in() {

  int i, j, check;
  
  static int		/* various state flags */
    curr_p_stage = 0,
    curr_p_len = 0,
    curr_p_index = 0, 
    curr_p_type = 0,
    curr_p_num = 0;
  static un_char header[4];
  extern int breakout_char;
  static int breakout = 0;
  
  if (p_in_num > 14 || !serial_in.size)
    return; /* packet window is full */
  
  while (serial_in.size) {  
    DEBUG_STATE(stderr, "d_s_i state %d len %d ind %d type %d num %d\n", 
		curr_p_stage, curr_p_len, curr_p_index, curr_p_type,
		curr_p_num);  
    switch (curr_p_stage) {
    case 0:			/* waiting for packet header */
      i = get_serial();
      if ( i < 0) break;
      if (i < 'A' || i > ('A'+MAX_TYPE)) { /* noise */
	noise(i);
	if ( i == (breakout_char^byte_shift)) {
	  ++breakout;
	  if (breakout == 5)
	    do_shutdown = 1;
	}
	else if ( i == ('1' ^ byte_shift) && breakout == 4) {
	  curr_p_stage = 4;
	  inhabit_send = 1;	/* Don't send packets while in this */
				/* mode. */
	  breakout = 0;
	}
	else breakout = 0;
	break;
      }
				/* ok. for now we will assume that it */
				/* is a header. */ 
      curr_p_index = 0;
      header[curr_p_index++] = i;
      curr_p_stage = 1;
      break;
    case 1:			/* read header */
      i = get_serial();
      if (i < 0) 
	break;
      header[curr_p_index++] = i;
      if (curr_p_index < 4) 
	break;			/* more to read yet */
				/* Ok. We have the whole header. Check */
				/* the checksum */ 
      i = update_crc(update_crc(update_crc(0, header[0]&in_mask),
				header[1]&in_mask), header[2]&in_mask); 
      if (!check_match( i & in_mask,header[3])) {
	DEBUG_CHECK(stderr, "%s:d_s_i: check calc %x, read %x\n", 
		    term_server, i, header[3]);
				/* checksum failed. treat it as noise */
				/* and try again */ 
	for (i =0; i < 4;++i)
	  noise(header[i]);
	curr_p_stage = 0;
	break;
      }
				/* ok. checksum was good. see what */
				/* type packet is */ 
      if (header[0] == 'D') {	/* it is an ack */
				/* we do a more strigent checksum here */
				/* cos bad acks are a real pain in the butt */
	DEBUG_SER(stderr, "Got ack.\n");
	if (!check_match((header[0] ^ header[1]) & in_mask,
			 header[2]&in_mask)) {
	  DEBUG_CHECK(stderr,
		      "d_s_i: secondary. calc %x, read %x\n", 
		      header[0] ^ header[1], header[2]);
				/* heh heh. bad secondary checksum. */
				/* lose that sucker */ 
	  curr_p_stage = 0;
	  break;
	}
	
	do_ack(header[1]);
	curr_p_stage = 0;
	break;
      }			
				/* see if we want the packet */
      i = (header[2] - p_in_e) & 31;
      if ( i > 16) {
	DEBUG_SER(stderr, "d_s_i: Got old packet %d\n", i);
				/* an old packet. just lose it */
	send_ack(header[2]);
	curr_p_stage = 3;
	curr_p_len = header[1] + 2;
	break;
      }	
      curr_p_num = header[2] & 31;
      if (p_in[curr_p_num].type >= 0) {
	DEBUG_SER(stderr, "Got dup packet\n");
				/* Hmm. We think we already have it . */
				/* check it */ 
	if (header[1] != p_in[curr_p_num].len) {
				/* We have a different length !! */
	  alert("Duplicate packet received with a different\n");
	  alert("length. sigh \n");
				/* just ignore it for now. Will HAVE */
				/* to be fixed */ 
	  curr_p_stage = 0;
	  break;
	}
				/* discard following data and checksum */
	curr_p_stage = 3;
	curr_p_len = header[1] + 2;
	break;
      }
      curr_p_stage = 2;
      curr_p_index = 0;	
      curr_p_len = header[1] + 2; /* get checksum as well. */
      break;
    case 2:			/* read data */
      i = get_serial();
      if (i < 0)
	break;
      p_in[curr_p_num].data[curr_p_index ++ ] = i;
      if (curr_p_index < curr_p_len) 
	break; /* more to read yet */
      
				/* all read. now test the checksum */
      j = (p_in[curr_p_num].data[curr_p_len - 1] <<8) +
	p_in[curr_p_num].data[curr_p_len -2] ;
      
      check = check_sum(p_in[curr_p_num].data, curr_p_len-2, in_mask);
      
      if (!check_match(j, check) ) {
	DEBUG_CHECK(stderr, "d_s_i: main calced = %x, read = %x\n", 
		    check, j);
	
				/* failed checksum . sigh. back to beginning */
	for (i  =0; i < curr_p_len;++i)
	  noise(p_in[curr_p_num].data[i]);
	curr_p_stage = 0;
	break;
      }
      
				/* ok. got a complete packet. */
				/* whack it in list. This is */
				/* difficult. grin. */ 
      send_ack(curr_p_num);
      p_in[curr_p_num].type = header[0] - 'A';
      p_in[curr_p_num].len = curr_p_len - 2;
      
      ++p_in_num;
      DEBUG_LINK(stderr, "Packet %d added to in Q\n", curr_p_num);
      curr_p_stage = 0;
      break;    
    case 3:			/* discard */
      if (curr_p_index <= 0) {	/* sanity check */
	curr_p_stage  = 0;
	break;
      }
      if (get_serial() < 0)
	break;
      if (!--curr_p_index)
	curr_p_stage = 0;
      break;
    case 4:			/* This state is special. This is the */
				/* meta-state used by term to check */
				/* for characters that are getting */
				/* eaten, or to do other things that */
				/* aren't based on packets. */
      i = get_serial();
      if (i < 0) break;

      if (meta_state(i))
	break;
				/* Return to normal packet handleing. */
      inhabit_send = 0;
      curr_p_stage = 0;
      break;
    default:
      curr_p_stage = 0;
      break;
    }
  }
}

void do_ack(int num) {
  DEBUG_SER(stderr, "Got ack for packet %d\n", num);
  num &= 31;
  if (p_out[num].type < 0) {
    DEBUG_SER(stderr, "Was dup.\n");
    return;
  }
  p_out[num].type = -1;
  
  while (p_out[p_out_e].type < 0 && p_out_num > 0) {
    p_out_e = (p_out_e + 1 ) & 31;
    --p_out_num;
  }
}


void send_ack(int num) {
				/* yucky eh?? 4 bytes to transmit 1. */
				/* Oh well. I fix it in some other version */ 
				/* lets get it working first. ;) */
  DEBUG_SER(stderr, "Sending ack for %d\n", num);
  put_serial('D');
  put_serial(num);
  put_serial(num ^ 'D');
  put_serial( update_crc(update_crc(update_crc(0, 'D'),
				    num), num ^ 'D'));
}
 
int check_match(int check, int calc) {
  if (!seven_bit_in)
    return check == calc;
  
  return ((check & 127) == (calc & 127)) &&
    (((check >> 8) & 127) == ((calc>>8) & 127));
}

