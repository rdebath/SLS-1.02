# 1 "" 











static char	RCSid_[] = "$Source:";










#include <stdio.h>
#include "sun.h"

#define RIGHT		0x0	
#define DOWN		0x0	
#define UP			0x10	
#define LEFT		0x20	
#define SMALL		0x40	
#define ALIGNED	0x80	
#define SRC_COLOR	0x100	
#define NSRC		1		

extern int bit_debug;

#define dprintf	if(bit_debug)fprintf

static char nsrc[16] = {		
	0,0,0,0,
	0xf&~DST, 0xf&~DST, 0xf&~DST, 0xf&~DST,
	0xf&DST, 0xf&DST, 0xf&DST, 0xf&DST, 
	0xf, 0xf, 0xf, 0xf
	};

static char zsrc[16] = {		
	1,0,0,0,0,
	1,0,0,0,0,
	1,0,0,0,0,
	1 };	



#define ZERO(d,s)	(0)
#define NOR(d,s)	(~((d)|(s)))
#define AND2(d,s)	((d)&~(s))
#define NPROJ2(d,s)	(~(s))
#define AND1(d,s)	(~(d)&(s))
#define NPROJ1(d,s)	(~(d))
#define XOR(d,s)	((d)^(s))
#define NAND(d,s)	(~((d)&(s)))
#define AND(d,s)	((d)&(s))
#define NXOR(d,s)	(~((d)^(s)))
#define PROJ1(d,s)	((d))
#define OR2(d,s)	((d)|~(s))
#define PROJ2(d,s)	(s)
#define OR1(d,s)	(~(d)|(s))
#define OR(d,s)		((d)|(s))
#define ONE(d,s)	(~0)



#define SINC(x)	x++
#define DINC(x)	x++
#define SDEC(x)	x--
#define DDEC(x)	x--









#define Rop_s(op) \
   for (i=count; i>0; i--) {			\
      osrc = src;  SINC(src);					\
      *dst = op(*dst,(GETMSB(*osrc,lshift) | GETLSB(*src,rshift))) \
					& lmask | (*dst & ~lmask);\
      src += s_skip;  dst += d_skip;				\
      }



#define Rop_sC(op) \
   for (i=count; i>0; i--) {			\
      *DINC(dst) = op(*dst,color) & lmask | (*dst & ~lmask);\
      dst += d_skip;				\
      }



#define Rop_sa(op) \
   for (i=count; i>0; i--) {			\
      *DINC(dst) = op(*dst,*SINC(src)) & lmask | (*dst & ~lmask);\
      src += s_skip;  dst += d_skip;				\
      }



#define Rop_incr_a(op) \
   for (i=count; i>0; i--) {			\
      *DINC(dst) = op(*dst,*SINC(src)) & lmask | (*dst & ~lmask);\
      LOOP(words-2,*DINC(dst) = op(*dst,*SINC(src))); \
      *DINC(dst) = op(*dst,*SINC(src)) & rmask | (*dst & ~rmask);\
      src += s_skip;  dst += d_skip;				\
      }



#define Rop_decr_a(op) \
   for (i=count; i>0; i--) {			\
      *DDEC(dst) = op(*dst,*SDEC(src)) & rmask | (*dst & ~rmask);\
      LOOP(words-2,*DDEC(dst) = op(*dst,*SDEC(src))); \
      *DDEC(dst) = op(*dst,*SDEC(src)) & lmask | (*dst & ~lmask);\
      src += s_skip;  dst += d_skip;				\
      }



#define Rop_incr_m(op) \
   for (i=count; i>0; i--) {			\
      osrc = src;  SINC(src);					\
      *DINC(dst) = op(*dst,(GETMSB(*SINC(osrc),lshift) \
					| GETLSB(*SINC(src),rshift))) \
					& lmask | (*dst & ~lmask);\
      LOOP(words-2,*DINC(dst) = \
			op(*dst,GETMSB(*SINC(osrc),lshift) | GETLSB(*SINC(src),rshift)) \
			); \
      *DINC(dst) = op(*dst,(GETMSB(*SINC(osrc),lshift) \
					| GETLSB(*SINC(src),rshift))) \
					& rmask | (*dst & ~rmask);\
      src += s_skip;  dst += d_skip;				\
      }



#define Rop_C(op) \
   for (i=count; i>0; i--) {			\
      *DINC(dst) = op(*dst,color) & lmask | (*dst & ~lmask);\
      LOOP(words-2,*DINC(dst) = op(*dst,color)); \
      *DINC(dst) = op(*dst,color) & rmask | (*dst & ~rmask);\
      dst += d_skip;				\
      }



#define Rop_decr_m(op) \
   for (i=count; i>0; i--) {			\
      osrc = src;  SDEC(osrc);					\
      *DDEC(dst) = op(*dst,(GETMSB(*SDEC(osrc),lshift) \
					| GETLSB(*SDEC(src),rshift))) \
					& rmask | (*dst & ~rmask);\
      LOOP(words-2,*DDEC(dst) = \
			op(*dst,GETMSB(*SDEC(osrc),lshift) | GETLSB(*SDEC(src),rshift)) \
			); \
      *DDEC(dst) = op(*dst,(GETMSB(*SDEC(osrc),lshift) \
					| GETLSB(*SDEC(src),rshift))) \
					& lmask | (*dst & ~lmask);\
      src += s_skip;  dst += d_skip;				\
      }

# 188 "" 

#define LOOP(n,s) {\
    register int cnt;\
    for (cnt=(n); cnt>0; cnt--) {\
       s;\
		} \
    }




#define SWITCH(op,func) \
		switch(op&0xf) { \
			case OPCODE(ZERO(DST,SRC)):	func(ZERO);		break; \
			case OPCODE(NOR(DST,SRC)):		func(NOR);		break; \
			case OPCODE(AND2(DST,SRC)):	func(AND2);		break; \
			case OPCODE(NPROJ2(DST,SRC)):	func(NPROJ2);	break; \
			case OPCODE(AND1(DST,SRC)):	func(AND1);		break; \
			case OPCODE(NPROJ1(DST,SRC)):	func(NPROJ1);	break; \
			case OPCODE(XOR(DST,SRC)):	 	func(XOR);		break; \
			case OPCODE(NAND(DST,SRC)):	func(NAND);		break; \
			case OPCODE(AND(DST,SRC)):		func(AND);		break; \
			case OPCODE(NXOR(DST,SRC)):	func(NXOR);		break; \
			case OPCODE(PROJ1(DST,SRC)):	func(PROJ1);	break; \
			case OPCODE(OR2(DST,SRC)):		func(OR2);		break; \
			case OPCODE(PROJ2(DST,SRC)):	func(PROJ2);	break; \
			case OPCODE(OR1(DST,SRC)):	 	func(OR1);		break; \
			case OPCODE(OR(DST,SRC)):		func(OR);		break; \
			case OPCODE(ONE(DST,SRC)):		func(ONE);		break; \
			}

#define XWITCH(op,func)		



mem_rop(dst_map,x_dst,y_dst,wide,high,op,src_map,x_src,y_src)
BITMAP *dst_map;				
BITMAP *src_map;				
int x_dst,y_dst;				
int x_src,y_src;				
int wide,high;					
int op;							
   {
	register DATA *dst;		
	register DATA *src;		
	register DATA *osrc;		

	register int s_offset;			
	register int d_skip;				
	register int s_skip;				
	register int count=high;		
   register int words;				
   register DATA lmask;				
   register DATA rmask;				
	register int lshift;				
	register int rshift;				
	register int i;					
	register DATA color;				
	register int mode=0;				
	int depth = dst_map -> depth;	

# 252 "" 


	

	if (!src_map && depth>1) {
		switch (OPCODE(op)) {
			case OPCODE(~SRC):
			case OPCODE(0):
			case OPCODE(~SRC | DST):
			case OPCODE(~SRC | ~DST):
			case OPCODE(DST & ~SRC):
			case OPCODE(~DST & ~SRC):
			case OPCODE(~SRC ^ DST):	
				color = GETBCOLOR(op);
				op = rop_invert(op);
				break;
			default:							
				color =  GETFCOLOR(op);
				break;
			}
		mode = SRC_COLOR;
		color = color | (color<<8) | (color<<16) | (color<<24);
		}
	

	if (depth==1)
		if (!src_map)
			op = GETFCOLOR(op) ? nsrc[0xf&op] : nsrc[0xf&rop_invert(op)];	
		else
			op = GETFCOLOR(op) ? op : rop_invert(op);		

	

	if (src_map && zsrc[op&0xf]) {
		fprintf(stderr,"op=%d, setting src_map->NULL\n",op&0xf);
		src_map == BIT_NULL;							
		}

	if (src_map &&  dst_map && depth != src_map->depth) {
		fprintf(stderr,"Incompatable depths: %d -> %d\n",
					src_map->depth, depth);
		return(-1);
		}



#ifndef NOCLIP
	
	

	if (wide<0) {
		dprintf(stderr,"Clip: w<0 (%d)\n",wide);
		x_dst += wide;
		wide = - wide;
		}

	if (count<0) {
		y_dst += count;
		count = - count;
		dprintf(stderr,"Clip: h<0 (%d)\n",count);
		}

	

   if (x_dst < 0) {
		dprintf(stderr,"Clip: x_dst<0 (%d)\n",x_dst);
		if (src_map)
			x_src -= x_dst;
		wide += x_dst;
		x_dst = 0;
		}

   if (y_dst < 0) {
		dprintf(stderr,"Clip: y_dst<0 (%d)\n",y_dst);
		if (src_map)
			y_src -= y_dst;
		count += y_dst;
		y_dst = 0;
		}

	

	if (src_map) {
		if (x_src < 0) {
			dprintf(stderr,"Clip: x_src<0 (%d)\n",x_src);
			x_dst -= x_src;
			wide += x_src;
			x_src = 0;
			}

		if (y_src < 0) {
			dprintf(stderr,"Clip: y_src<0 (%d)\n",y_src);
			y_dst-=y_src;
			count+=y_src;
			y_src=0;
			}
			
		if ((i = x_src+wide - src_map->wide) > 0) {
			dprintf(stderr,"Clip: wide too big for src (%d->%d)\n",wide,wide-i);
			wide -= i;
			}

		if ((i = y_src+count - src_map->high) > 0) {
			dprintf(stderr,"Clip: high too big for src (%d->%d)\n",count,count-i);
			count -= i;
			}
		}

	if ((i = x_dst + wide - dst_map->wide) > 0) {
		dprintf(stderr,"Clip: wide too big for dst_map (%d->%d)\n",wide,wide-i);
		wide -= i;
		}
	if ((i = y_dst + count - dst_map->high) > 0) {
		dprintf(stderr,"Clip: high too big for dst_map (%d->%d)\n",count,count-i);
		count -= i;
		}

	if (wide<1 || count < 1) {
		dprintf(stderr,"Clip: high or wide < 1 (%d,%d)\n",wide,count);
		return(-1);
		}

	

#endif

	x_dst += dst_map->x0;
	y_dst += dst_map->y0;

	

	wide *= depth;	
	x_dst *= depth;

	

	if (DOFLIP && dst_map->primary->type&_FLIP) {
		flip(dst_map->data,BIT_LINE(dst_map)*dst_map->primary->high);
		dst_map->primary->type &= 3;
		}

	words = ((x_dst+wide-1)>>LOGBITS) - (x_dst>>LOGBITS) + 1;
	lmask = GETLSB((DATA) ~0, (x_dst&BITS));
	rmask = GETMSB((DATA) ~0, (BITS - ((x_dst+wide-1)&BITS)));
	dst = dst_map->data + BIT_LINE(dst_map)*y_dst + (x_dst>>LOGBITS);
	d_skip = BIT_LINE(dst_map);		

   if (src_map) {

		

		if (DOFLIP && src_map->primary->type&_FLIP) {
			flip(src_map->data,BIT_LINE(src_map)*src_map->primary->high);
			src_map->primary->type &= 3;
			}

		x_src += src_map->x0;
		x_src *= depth;
		y_src += src_map->y0;
		src = src_map->data + BIT_LINE(src_map)*y_src + (x_src>>LOGBITS);
		s_skip = BIT_LINE(src_map);

		

		lshift = (x_src&BITS) - (x_dst&BITS);
		if (lshift > 0) {
			rshift = (BITS+1) - lshift;
			}
		else  if (lshift < 0){
			rshift = -lshift;
			lshift = (BITS+1) - rshift;
			src--;
			}
		else {
			mode |= ALIGNED;
			}

		

		if (src_map->data == dst_map->data) {
			if (y_dst>y_src)
				mode |= UP;
			if (x_dst>x_src)
				mode |= LEFT;
			}

		}		

	if (words <=1 ) {
		mode |= SMALL;
		lmask &= rmask;
		}

	









   switch(mode) {

	case SRC_COLOR:						
		d_skip -= words;
		SWITCH(op,Rop_C);
      break;

	case SRC_COLOR|SMALL:				
		d_skip -= words;
		SWITCH(op,Rop_sC);
      break;

	case RIGHT|DOWN:						
		d_skip -= words;
		s_skip -= (words + 1);
		SWITCH(op,Rop_incr_m);
      break;

	case RIGHT|DOWN|SMALL:				
	case LEFT|DOWN|SMALL:
		d_skip -= words - 1;
		s_skip -= words;
		SWITCH(op,Rop_s);
      break;

	case RIGHT|DOWN|ALIGNED:			
		d_skip -= words;
		s_skip -= words;
		SWITCH(op,Rop_incr_a);
      break;

	case RIGHT|DOWN|SMALL|ALIGNED:	
	case LEFT|DOWN|SMALL|ALIGNED:	
		d_skip -= words;
		s_skip -= words;
		SWITCH(op,Rop_sa);
      break;

	case RIGHT|UP:							
		dst += d_skip*(count-1);
		d_skip = - (d_skip + words);
		if (src_map) {
			src += s_skip*(count-1);
			s_skip = - (s_skip + words + 1);
			}
		SWITCH(op,Rop_incr_m);
      break;

	case RIGHT|UP|SMALL:					
	case LEFT|UP|SMALL:					
		dst += d_skip*(count-1);
		d_skip = - (d_skip + words - 1);
		if (src_map) {
			src += s_skip*(count-1);
			s_skip = - (s_skip + words);
			}
		SWITCH(op,Rop_s);
      break;

	case RIGHT|UP|ALIGNED:				
		dst += d_skip*(count-1);
		d_skip = - (d_skip + words);
		if (src_map) {
			src += s_skip*(count-1);
			s_skip = - (s_skip + words);
			}
		SWITCH(op,Rop_incr_a);
      break;

	case RIGHT|UP|SMALL|ALIGNED:		
	case LEFT|UP|SMALL|ALIGNED:
		dst += d_skip*(count-1);
		d_skip = - (d_skip + words);
		if (src_map) {
			src += s_skip*(count-1);
			s_skip = - (s_skip + words);
			}
		SWITCH(op,Rop_sa);
      break;

	case LEFT|DOWN:						
		dst += words - 1;
		d_skip += words;
		if (src_map) {
			src += words;
			s_skip += words;
			}
		SWITCH(op,Rop_decr_m);
      break;

	case LEFT|DOWN|ALIGNED:				
		dst += words - 1;
		d_skip += words;
		if (src_map) {
			src += words - 1;
			s_skip += words;
			}
		SWITCH(op,Rop_decr_a);
      break;

	case UP|LEFT:							
		dst += d_skip*(count-1) + words - 1;
		d_skip = - (d_skip - words);
		if (src_map) {
			src += s_skip*(count-1) + words;
			s_skip = -(s_skip - words);
			}
		SWITCH(op,Rop_decr_m);
      break;

	case UP|LEFT|ALIGNED:				
		dst += d_skip*(count-1) + words - 1;
		d_skip = - (d_skip - words);
		if (src_map) {
			src += s_skip*count + words - 1;
			s_skip = -(s_skip - words );
			}
		SWITCH(op,Rop_decr_a);
      break;

	default:
		dprintf(stderr,"Invalid direction: 0x%x\n",mode);
		break;
		}
	return(0);
  	}
