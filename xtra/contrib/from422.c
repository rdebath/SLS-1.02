/* 
 *	from422 - 
 *		Convert from a CCIR-601 4:2:2 image to an IRIS image.
 *
 *	 	    		Paul Haeberli - 1988
 *
 *	data comes in the order:  cb y cr y cb y cr y . . . 
 *	
 *	Frame look like this:
 *
 *	24 blanking lines.
 *	288 lines in first field.
 *	25 blanking lines.
 *	288 lines in second field.
 *
 */
#include "image.h"
#include "stdio.h"

#define MAXIWIDTH	8192

unsigned short rbuf[MAXIWIDTH];
unsigned short gbuf[MAXIWIDTH];
unsigned short bbuf[MAXIWIDTH];
unsigned short ibuf[MAXIWIDTH];

main(argc,argv)
int argc;
char **argv;
{
    if(argc<3) {
	fprintf(stderr,"usage: from422 inimage.422 outimage.rgb\n");
	exit(1);
    }
    from422(argv[1],argv[2]);
}

from422(iname,tname)
char *iname, *tname;
{
    int xsize, ysize, zsize;
    int y;
    IMAGE *iimage, *oimage;

    iimage = iopen(iname,"r");
    if(!iimage) {
	fprintf(stderr,"from422: can't open input file %s\n",iname);
	exit(1);
    }
    xsize = iimage->xsize/2;
    ysize = iimage->ysize;
    oimage = iopen(tname,"w",RLE(1),3,xsize,ysize,3);
    for(y=0; y<ysize; y++) {
	getrow(iimage,ibuf,y,0);
	cvtfrom422(ibuf,rbuf,gbuf,bbuf,xsize);
	putrow(oimage,rbuf,y,0);
	putrow(oimage,gbuf,y,1);
	putrow(oimage,bbuf,y,2);
    }
    iclose(oimage);
}

cvtfrom422(in,r,g,b,n)
register unsigned short *in, *r, *g, *b;
register int n;
{
    int i,lum;
    unsigned short y, cr, cb;
    unsigned short *sptr, *rptr, *gptr, *bptr;

/* reconstruct color for the whole line */
    sptr = in;
    rptr = r;
    gptr = g;
    bptr = b;
    i = n/2;
    while(i--) {
	cvt422torgb(sptr[1],sptr[0],sptr[2],rptr,gptr,bptr);
	rptr += 2;
	gptr += 2;
	bptr += 2;
	sptr += 4;
    }

/* lerp between every other color pixel */
    lerpem(r,n);
    lerpem(g,n);
    lerpem(b,n);

/* force the lumanence to be right */
    sptr = in+1;
    rptr = r;
    gptr = g;
    bptr = b;
    i = n;
    while(i--) {
	lum = ((*sptr-16)*255)/219;
	dosetlum(rptr++,gptr++,bptr++,lum);
	sptr += 2;
    }
}

lerpem(sptr,n)
register unsigned short *sptr;
register int n;
{
    n /= 2;
    while(n--) {
	sptr[1] = ((sptr[0]+sptr[2])+1)>>1;
	sptr += 2;
    }
}

rgbto422(r,g,b,y,cb,cr)
int r, g, b;
unsigned short *y, *cb, *cr;
{
    register int lum;

    lum = 77*r+150*g+29*b;
    r = r<<8;
    b = b<<8;
    *y = ((220*lum)>>16)+16;
    *cb = ((126*(b-lum))>>8) + 128; 
    *cr = ((160*(r-lum))>>8) + 128; 
}

cvt422torgb(y,cb,cr,r,g,b)
int y, cb, cr;
short *r, *g, *b;
{
    register int lum;
    int tr, tg, tb;

    cb -= 128;
    cr -= 128;
    y -= 16;
    cb = (cb*255)/126;
    cr = (cr*255)/160;
    y = (y*255)/219;
    tr = cr+y;
    tb = cb+y;
    tg = (255*y - 77*tr - 29*tb)/150;
    if(tr<0)
	*r = 0;
    else
	*r = tr;
    if(tg<0)
	*g = 0;
    else
	*g = tg;
    if(tb<0)
	*b = 0;
    else
	*b = tb;
#ifdef SDFSDF
    check(0,r);
    check(1,g);
    check(2,b);
    *r = y;
    *g = y;
    *b = y;
#endif
}

dosetlum(r,g,b,i)
unsigned short *r, *g, *b, i;
{
    register int lum, wantlum;
    register int whiteness;
    register int colorness;
    register int div;

    lum = (77*(*r) + 150*(*g) + 29*(*b))>>8;
    wantlum = i;
    if(wantlum<=lum) {
	if(lum>0) {
	    *r = ((*r) * wantlum)/lum;
	    *g = ((*g) * wantlum)/lum;
	    *b = ((*b) * wantlum)/lum;
	} else {
	    *r = 0;
	    *g = 0;
	    *b = 0;
	}
    } else {
	colorness = 255-wantlum;
	whiteness = 255*(wantlum-lum);
	div = 255-lum;
	*r = ((*r)*colorness + whiteness)/div;
	*g = ((*g)*colorness + whiteness)/div;
	*b = ((*b)*colorness + whiteness)/div;
    }
}

check(i,v)
int i,*v;
{
    if(*v<0)
	*v = 0;
    if(*v>255)
	*v = 255;
}
