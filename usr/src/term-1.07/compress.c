/*
** Copyright Michael O'Reilly. All rights reserved.
*/
#include "includes.h"
#include "debug.h"
				/* Following used for debugging. */
#if 0
#define ASSERT(a) \
	((a) ? 0: abort() )
#else
#define ASSERT(a)
#endif

#define BYTE_WIDTH_OUT	tok_byte_width_out
#define BYTE_MASK_OUT	tok_byte_mask_out

#define BYTE_MASK_IN	tok_byte_mask_in
#define BYTE_WIDTH_IN	tok_byte_width_in

int tok_byte_mask_out = 255,
  tok_byte_width_out = 8;
int tok_byte_mask_in = 255,
  tok_byte_width_in = 8;

int stat_comp_in = 0,
  stat_comp_out = 0,
  stat_uncomp_in = 0,
  stat_uncomp_out = 0;
/*
** token 256 is reserved to signal that dict clearing should take place.
** token 257 is reserved for future use.
**
** the maz token size is 16 bits.
*/

#define DICT_SIZE  8192
#define DICT_CLEAR_2 4096
#define DICT_CLEAR_2_TSIZE 13

/* #define DEBUG **/

typedef struct node {
    short   ch;			/* character for this node */
    struct node *next_node;	/* Pointer to the next link on this node. */
    struct node *child;		/* Pointer to a child of this node. */
    int     len;		/* You tell me.  */
    int     num;		/* token number for this node */
}                   NODE;

#ifdef DEBUG
FILE * stdprn = 0;
#endif
#if 0
extern  void * malloc (int);
extern  void * realloc (char *, int);
#endif

void clear_table (NODE *, int);
int  uncomp_dict[DICT_SIZE][2];
NODE comp_dict = {
    0, 0, 0, 0, 0
};

static int  c_token_mask,
            c_token_bits,
            unc_token_bits,
            unc_token_mask,

            comp_table_size,
            uncomp_table_size;
static int  stack[1024];

static int  stk_ptr = 0;
int compress_init (void) {
    int     i;
    if (comp_dict.child) {
	for (i = 0; i < 256; ++i)
	    clear_table (&(comp_dict.child[i]), 255);
    }
    else
	comp_dict.child = (NODE *) malloc (256 * sizeof (NODE));
    comp_dict.len = 256;
    comp_dict.ch = 0;
    comp_dict.num = 0;
    comp_dict.next_node = 0;	/* Only one NODE in this node. */
    for (i = 0; i < 256; ++i) {
	uncomp_dict[i][0] = comp_dict.child[i].ch = i;
	uncomp_dict[i][1] = -1;

	comp_dict.child[i].num = i;
	comp_dict.child[i].child = NULL;
	comp_dict.child[i].len = 0;
	comp_dict.child[i].next_node = &comp_dict.child[i+1];
    }
    comp_dict.child[255].next_node = (NODE *) 0;

    comp_table_size = 258;
    uncomp_table_size = 258;

    c_token_bits = 9;
    c_token_mask = 511;
    unc_token_bits = 9;
    unc_token_mask = 511;

#ifdef DEBUG
    if (!stdprn) stdprn = fopen ("compress.debug", "w");
    setbuf(stdprn , 0);
    fprintf(stdprn,"init\n");
#endif
    return 1;
}

void compress_shut (void) {
#ifdef DEBUG
    fclose (stdprn);
#endif
}

/* gets size bits from starting from bit 'curr' in the bit stream data */

unsigned int    get_token (un_char *data, int *curr, int max_size) {
    unsigned long   ret;
    int     byte,
            bit,
            read = 0,
            tmp;
    byte = *curr / BYTE_WIDTH_IN;
    bit = *curr % BYTE_WIDTH_IN;
    ret = 0;
    while (read < unc_token_bits && byte < max_size) {
	ret += ((data[byte++] & BYTE_MASK_IN) >> bit) << read;
	tmp = BYTE_WIDTH_IN - bit;
	read += tmp;
	bit = 0;
    }
    ret = ret & unc_token_mask;
    *curr += unc_token_bits;
    DEBUG_C(stderr, "c_got: %d\n", ret);
    return (unsigned int) ret;
}

void put_token (un_char *data, int *curr, unsigned int ch) {
    int     byte,
            bit;
    DEBUG_C(stderr, "c_put: %d\n", ch);
    byte = *curr / BYTE_WIDTH_OUT;
    bit = *curr % BYTE_WIDTH_OUT;
    ch = ch & c_token_mask;
    while (ch) {
	data[byte] |= (ch&BYTE_MASK_OUT) << bit;
	ch = ch >> (BYTE_WIDTH_OUT - bit);
	bit = 0;
	++byte;
    }
    *curr += c_token_bits;
}

/* Add a node to the list of free node's */
NODE * node_list = NULL;
void free_node (n)
NODE * n;
{
    n -> next_node = node_list;
    node_list = n;
}

/* Take a node of the list of free nodes, creating more nodes if there */
/* aren't enough to spare.. */
NODE * get_node (void) {
    NODE * n;
    if (!node_list) {
	n = (NODE *) malloc (sizeof (*n));
	if (!n) abort();	/* fatal */
	return n;
    }
    n = node_list;
    node_list = node_list -> next_node;
    return n;
}

/* Free this node, and any nodes pointed to by this node. */
void free_nodes(NODE *t) {
    if (!t) return;
    if (t->child) free_nodes(t->child);
    if (t->next_node) free_nodes(t->next_node);
    free_node(t);
    }

/* Clear all nodes larger than 'limit' */
/* The compression dictionary is a tree , built from a list of lists. */
 /* The tree is a 1-256 tree. (each node can have up to 256 children). */
 /* This is implemented as each node being a list of child pointers. */
 /* Each node has a l for the child node, and an n pointer for the */
 /* next node on this level. */

/* Thus, searching the tree consists of running down the 'n' list */
 /* untill the first character is matched, and then following the 'l' */
 /* link. Then running down the 'n' again, and so on. */
void clear_table (dict, limit)
NODE * dict;
int     limit;
{
    NODE * t, *t1, *tmp;
    /* If we have no dictionary do nothing. */
    if (!dict)
	return;
				/* This is fairly messy. We run across */
				/* the current node, and build a list */
				/* of all the nodes we still want. One */
				/* of the properties of the tree we */
				/* take advantage of that that all */
				/* lower nodes are guarenteed to */
				/* higher token numbers. */
    tmp = 0;
    for (t = dict -> child; t;)	/* Run across the current tree node. */
	if (t -> num > limit) {	/* If this node is too large... */
	    t1 = t;		/* rember current node, */
	    t = t->next_node;	/* move loop pointer to next one */
	    t1->next_node = 0;	/* and free just the one we had. */
	    free_nodes(t1);
	    }
	else {
	    clear_table (t, limit); /* Ok. Recurse through and look */
				    /* through the lower levels. */
	    t1 = t->next_node;	/* Move to next node,  */
	    t->next_node = tmp;	/* and put the current node on the */
				/* list of nodes to remember. */
	    tmp = t;
	    t = t1;
	    }

    dict->child = tmp;		/* ok. We built the list, now store it. */
}

int     compress (un_char *outpath, int maxlen, int prefix) {
  int     out_fd = 0;
  int     suffix;
  int    *kludge;
  NODE * prfix, *pt;
  int     i,
  lim;
  extern int stat_rare_out;

  if ((prefix) < 0)		/* Get a byte if we can. */
    return 0;			/* we can't, so we managed to compress */
				/* zero bytes..  */

  ++stat_comp_in;
  prfix = &comp_dict.child[prefix]; /* Move to right point in dictionary.. */
  
  kludge = (int *) outpath;	/* this relies on 4 byte int's */
  for (i = 0; i < 65; ++i)	/* clear the output buffer (need to do */
				/* this, as the token out routines use */
				/* OR'ing). Assumes the buffer can */
				/* hold 260 characters. Thanks to */
				/* Janne Sinkkonen for pointing this */
				/* out. *blush*  */
    kludge[i] = 0;
  
  
  if (comp_table_size >= DICT_SIZE - 2) { /* if table is full. */
#if 0
    fprintf(stderr, "Clearing dict table\n");
#endif
    for (i = 0; i < 256; ++i)
      clear_table (&(comp_dict.child[i]), DICT_CLEAR_2 - 1);
    comp_table_size = DICT_CLEAR_2;
    put_token (outpath, &out_fd, 256); /* Clear table. */
    c_token_mask = 511;
    c_token_bits = 9;
  }
  lim = (maxlen - 4) * BYTE_WIDTH_OUT; /* The maximum number of bits to */
				/* output. */
  
#ifdef DEBUG
  fprintf(stdprn, "Setting lim to %d\n", lim);
#endif
  while (out_fd < lim) {	/* while we have output less then */
				/* 'lim' bits. */
    suffix = get_client_byte(); /* get the next byte. */
    ++stat_comp_in;
    ++stat_rare_out;

    DEBUG_C(stderr, "Got byte == %d\n", suffix);

    if (suffix < 0)		/* We has nothing left to compress so exit. */
      break;
    suffix &= 255;		/* Make it 8 bits. Not needed */
				/* theoretically, but.. */
    for (pt = prfix -> child; pt; pt = pt -> next_node)
      if (pt -> ch == suffix)
	break;
    
    if (pt) {			/* found */
#ifdef DEBUG
      fprintf(stdprn, " found\n");
#endif
      prfix = pt;		/* Just remember new prefix. */
    }
    else {			/* Ok. We have no token for this */
				/* sequence, so we make a new token */
				/* for it. */
      if (comp_table_size < DICT_SIZE - 1) { /* if the dict isn't */
					     /* full.. */
	pt = get_node ();	/* Get a new node.. */
	pt -> next_node = prfix -> child; /* and put it into tree. */
	prfix -> child = pt;
	pt -> ch = suffix;
	pt -> child = NULL;	/* No child strings. */
	pt -> num = comp_table_size++; /* Set the token number. */
      }
				/* Seeing as we are useing explicit */
				/* signaling to increase the token */
				/* size, we might as well put it off */
				/* for as long as we can. */
      while (prfix->num > c_token_mask) { /* do we need to increase token */
				/* size?? */
	put_token(outpath, &out_fd, 257);
	c_token_mask = (c_token_mask <<1) + 1;
	c_token_bits ++;
      }

      put_token (outpath, &out_fd, prfix -> num); /* we output the */
						  /* token as well. */
      prfix = &comp_dict.child[suffix]; /* and init prfix to the new value. */
    }
  }

  while (prfix->num > c_token_mask) { /* Just in case we need to increase */
    put_token(outpath, &out_fd, 257); /* the token size. */
    c_token_mask = (c_token_mask <<1) + 1;
    c_token_bits ++;
  }
  put_token (outpath, &out_fd, prfix -> num); /* Put the last token we */
					      /* had out. */
  stat_comp_out += (out_fd+BYTE_WIDTH_OUT - 1) / BYTE_WIDTH_OUT;
  return out_fd;
}

int uncompress(un_char *data, int len, un_char *outpath) {
int out_fd = 0, in_fd = 0;
static int token, oldtoken, newtoken, finaltoken;
int size;
static int started = 0;

#ifdef DEBUG
	fprintf(stdprn, "Uncompress\n");
#endif
	if (uncomp_dict == NULL) return -1;
	stk_ptr = in_fd = out_fd = 0;
	size = len * BYTE_WIDTH_IN ;
	if (!started) {
	        ASSERT(in_fd == 0);
		token = get_token(data , &in_fd, len);
		oldtoken= token;
		finaltoken = token;
		outpath[out_fd++]= token;
#ifdef DEBUG
		fprintf(stdprn, "[%d]", token);
#endif
		}

	if (started == 0) started = 2;
	else started = 1;
#ifdef DEBUG
	fprintf(stdprn, "Uncompress 1\n");
#endif
	while (in_fd + unc_token_bits <= size) {
        	token = get_token(data, &in_fd, len);
		ASSERT(in_fd <= size);
#ifdef DEBUG
	fprintf(stdprn, "new token == %d\n", token);
#endif
		if (token == 256) {
			uncomp_table_size = DICT_CLEAR_2;
			unc_token_bits = 9;
			unc_token_mask = 511;
			continue;
			}
		else if (token == 257) { /* increase token size by one bit. */
			unc_token_bits ++;
			unc_token_mask <<=1;
			++unc_token_mask;
			continue;
			}
		newtoken = token;

		if (uncomp_table_size <= newtoken) {
			stack[stk_ptr ++ ] = finaltoken;
			ASSERT(stk_ptr > 0);
			ASSERT(stk_ptr < 1024);
#ifdef DEBUG
		fprintf(stdprn, "[%d]", finaltoken);
#endif
			token = oldtoken;
			}
		while (uncomp_dict[token][1]>=0) {
		  stack[stk_ptr++] = uncomp_dict[token][1];
		  ASSERT(stk_ptr < 1024);
		  ASSERT(stk_ptr > 0);
		  token = uncomp_dict[token][0];
		}
		
		outpath[out_fd++] = token;
		ASSERT(out_fd < 2048);
		ASSERT(out_fd > 0);
#ifdef DEBUG
		fprintf(stdprn, "[ %d]", token);
#endif
		finaltoken = token;
		while (stk_ptr) {
		  outpath[out_fd++] = stack[--stk_ptr];
		  ASSERT(out_fd < 2048);
		  ASSERT(out_fd > 0);
			ASSERT(stk_ptr >= 0);
#ifdef DEBUG
			fprintf(stdprn, "[%d]", stack[stk_ptr]);
#endif
			}
		ASSERT(uncomp_table_size < (DICT_SIZE + 1));
		if (uncomp_table_size <DICT_SIZE) {
/*		        if (oldtoken == token) abort(); */
			uncomp_dict[uncomp_table_size][0] = oldtoken;
			uncomp_dict[uncomp_table_size][1] = token;


#ifdef DEBUG
			fprintf(stdprn, "Adding %d %d to table as %d\n", oldtoken, token, uncomp_table_size);
#endif
			if (started ==1 ) started = 2;
			else ++uncomp_table_size;
			}
		oldtoken = newtoken;
		}
	return out_fd;
	}


