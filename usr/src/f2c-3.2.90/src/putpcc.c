/****************************************************************
Copyright 1990 by AT&T Bell Laboratories and Bellcore.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the names of AT&T Bell Laboratories or
Bellcore or any of their entities not be used in advertising or
publicity pertaining to distribution of the software without
specific, written prior permission.

AT&T and Bellcore disclaim all warranties with regard to this
software, including all implied warranties of merchantability
and fitness.  In no event shall AT&T or Bellcore be liable for
any special, indirect or consequential damages or any damages
whatsoever resulting from loss of use, data or profits, whether
in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of
this software.
****************************************************************/

/* INTERMEDIATE CODE GENERATION FOR S. C. JOHNSON C COMPILERS */
/* NEW VERSION USING BINARY POLISH POSTFIX INTERMEDIATE */

#include "defs.h"
#include "pccdefs.h"
#include "output.h"		/* for nice_printf */
#include "names.h"
#include "p1defs.h"

Addrp realpart();
LOCAL Addrp intdouble(), putcx1(), putcxeq (), putch1 (), putchop ();
LOCAL putct1 ();

expptr putcxop();
LOCAL expptr putcall (), putmnmx (), putcheq(), putcat ();
LOCAL expptr putaddr(), putchcmp (), putpower(), putop();
LOCAL expptr putcxcmp ();
expptr imagpart();
ftnint lencat();

#define FOUR 4
extern int ops2[];
extern int types2[];
extern int proc_argchanges, proc_protochanges;

#define P2BUFFMAX 128

/* Puthead -- output the header information about subroutines, functions
   and entry points */

puthead(s, class)
char *s;
int class;
{
	if (headerdone == NO) {
		if (class == CLMAIN)
			s = "MAIN__";
		p1put_head (class, s);
		headerdone = YES;
		}
}

putif(p, else_if_p)
 register expptr p;
 int else_if_p;
{
	register int k;

	if( ( k = (p = fixtype(p))->headblock.vtype) != TYLOGICAL)
	{
		if(k != TYERROR)
			err("non-logical expression in IF statement");
		}
	else {
		p = putx(p);
		if (else_if_p)
		    p1put_elif(p);
		else
		    p1put_if(p);
	}
	frexpr(p);
}


putexpr(p)
expptr p;
{
	putex1(p);
}


putout(p)
expptr p;
{
	p1put_expr (p);

/* Used to make temporaries in holdtemps available here, but they */
/* may be reused too soon (e.g. when multiple **'s are involved). */
} /* putout */



putcmgo(index, nlab, labs)
expptr index;
int nlab;
struct Labelblock *labs[];
{
	if(! ISINT(index->headblock.vtype) )
	{
		execerr("computed goto index must be integer", CNULL);
		return;
	}

	p1put_comp_goto (index, nlab, labs);
}

expptr putx(p)
expptr p;
{
	int opc;
	int k;

	if (p)
	  switch(p->tag)
	{
	case TERROR:
		break;

	case TCONST:
		switch(p->constblock.vtype)
		{
		case TYLOGICAL:
		case TYLONG:
		case TYSHORT:
			break;

		case TYADDR:
			break;
		case TYREAL:
		case TYDREAL:

/* Don't write it out to the p2 file, since you'd need to call putconst,
   which is just what we need to avoid in the translator */

			break;
		default:
			p = putx( putconst(p) );
			break;
		}
		break;

	case TEXPR:
		switch(opc = p->exprblock.opcode)
		{
		case OPCALL:
		case OPCCALL:
			if( ISCOMPLEX(p->exprblock.vtype) )
				p = putcxop(p);
			else	p = putcall(p, (Addrp *)NULL);
			break;

		case OPMIN:
		case OPMAX:
			p = putmnmx(p);
			break;


		case OPASSIGN:
			if(ISCOMPLEX(p->exprblock.leftp->headblock.vtype)
			    || ISCOMPLEX(p->exprblock.rightp->headblock.vtype)) {
				(void) putcxeq(p);
				p = ENULL;
			} else if( ISCHAR(p) )
				p = putcheq(p);
			else
				goto putopp;
			break;

		case OPEQ:
		case OPNE:
			if( ISCOMPLEX(p->exprblock.leftp->headblock.vtype) ||
			    ISCOMPLEX(p->exprblock.rightp->headblock.vtype) )
			{
				p = putcxcmp(p);
				break;
			}
		case OPLT:
		case OPLE:
		case OPGT:
		case OPGE:
			if(ISCHAR(p->exprblock.leftp))
			{
				p = putchcmp(p);
				break;
			}
			goto putopp;

		case OPPOWER:
			p = putpower(p);
			break;

		case OPSTAR:
			/*   m * (2**k) -> m<<k   */
			if(INT(p->exprblock.leftp->headblock.vtype) &&
			    ISICON(p->exprblock.rightp) &&
			    ( (k = log2(p->exprblock.rightp->constblock.Const.ci))>0) )
			{
				p->exprblock.opcode = OPLSHIFT;
				frexpr(p->exprblock.rightp);
				p->exprblock.rightp = ICON(k);
				goto putopp;
			}

		case OPMOD:
			goto putopp;
		case OPPLUS:
		case OPMINUS:
		case OPSLASH:
		case OPNEG:
		case OPNEG1:
		case OPABS:
		case OPDABS:
			if( ISCOMPLEX(p->exprblock.vtype) )
				p = putcxop(p);
			else	goto putopp;
			break;

		case OPCONV:
			if( ISCOMPLEX(p->exprblock.vtype) )
				p = putcxop(p);
			else if( ISCOMPLEX(p->exprblock.leftp->headblock.vtype) )
			{
				p = putx( mkconv(p->exprblock.vtype,
				    realpart(putcx1(p->exprblock.leftp))));
			}
			else	goto putopp;
			break;

		case OPNOT:
		case OPOR:
		case OPAND:
		case OPEQV:
		case OPNEQV:
		case OPADDR:
		case OPPLUSEQ:
		case OPSTAREQ:
		case OPCOMMA:
		case OPQUEST:
		case OPCOLON:
		case OPBITOR:
		case OPBITAND:
		case OPBITXOR:
		case OPBITNOT:
		case OPLSHIFT:
		case OPRSHIFT:
		case OPASSIGNI:
		case OPIDENTITY:
		case OPCHARCAST:
		case OPMIN2:
		case OPMAX2:
		case OPDMIN:
		case OPDMAX:
putopp:
			p = putop(p);
			break;

		default:
			badop("putx", opc);
			p = errnode ();
		}
		break;

	case TADDR:
		p = putaddr(p);
		break;

	default:
		badtag("putx", p->tag);
		p = errnode ();
	}

	return p;
}



LOCAL expptr putop(p)
expptr p;
{
	expptr lp, tp;
	int pt, lt;
	int comma;

	switch(p->exprblock.opcode)	/* check for special cases and rewrite */
	{
	case OPCONV:
		pt = p->exprblock.vtype;
		lp = p->exprblock.leftp;
		lt = lp->headblock.vtype;

/* Simplify nested type casts */

		while(p->tag==TEXPR && p->exprblock.opcode==OPCONV &&
		    ( (ISREAL(pt)&&ISREAL(lt)) ||
		    (INT(pt)&&(ONEOF(lt,MSKINT|MSKADDR|MSKCHAR|M(TYSUBR)))) ))
		{
#if SZINT < SZLONG
			if(lp->tag != TEXPR)
			{
				if(pt==TYINT && lt==TYLONG)
					break;
				if(lt==TYINT && pt==TYLONG)
					break;
			}
#endif


			if(pt==TYDREAL && lt==TYREAL)
			{
				if(lp->tag==TEXPR &&
				    lp->exprblock.opcode==OPCONV &&
				    lp->exprblock.leftp->headblock.vtype==TYDREAL)
				{
					lp->exprblock.leftp =
						putx(lp->exprblock.leftp);
					return p;
				}
				else break;
			}


			if(lt==TYCHAR && lp->tag==TEXPR &&
			    lp->exprblock.opcode==OPCALL)
			{

/* May want to make a comma expression here instead.  I had one, but took
   it out for my convenience, not for the convenience of the end user */

				putout (putcall (lp, (Addrp *) &(p ->
				    exprblock.leftp)));
				return putop (p);
			}
			if (lt == TYCHAR) {
				p->exprblock.leftp = putx(p->exprblock.leftp);
				return p;
				}
			free( (charptr) p );
			p = lp;
			if (p->tag != TEXPR)
				goto retputx;
			pt = lt;
			lp = p->exprblock.leftp;
			lt = lp->headblock.vtype;
		} /* while */
		if(p->tag==TEXPR && p->exprblock.opcode==OPCONV)
			break;
 retputx:
		return putx(p);

	case OPADDR:
		comma = NO;
		lp = p->exprblock.leftp;
		free( (charptr) p );
		if(lp->tag != TADDR)
		{
			tp = (expptr)
			    Mktemp(lp->headblock.vtype,lp->headblock.vleng);
			p = putx( mkexpr(OPASSIGN, cpexpr(tp), lp) );
			lp = tp;
			comma = YES;
		}
		if(comma)
			p = mkexpr(OPCOMMA, p, putaddr(lp));
		else
			p = (expptr)putaddr(lp);
		return p;

	case OPASSIGN:
	case OPASSIGNI:
	case OPLT:
	case OPLE:
	case OPGT:
	case OPGE:
	case OPEQ:
	case OPNE:
	    ;
	}

	if( ops2[p->exprblock.opcode] <= 0)
		badop("putop", p->exprblock.opcode);
	p -> exprblock.leftp = putx (p -> exprblock.leftp);
	if (p -> exprblock.rightp)
	    p -> exprblock.rightp = putx (p -> exprblock.rightp);
	return p;
}

LOCAL expptr putpower(p)
expptr p;
{
	expptr base;
	Addrp t1, t2;
	ftnint k;
	int type;
	char buf[80];			/* buffer for text of comment */

	if(!ISICON(p->exprblock.rightp) ||
	    (k = p->exprblock.rightp->constblock.Const.ci)<2)
		Fatal("putpower: bad call");
	base = p->exprblock.leftp;
	type = base->headblock.vtype;
	t1 = Mktemp(type, PNULL);
	t2 = NULL;

	free ((charptr) p);
	p = putassign (cpexpr((expptr) t1), base);

	sprintf (buf, "Computing %d%s power", k, k == 2 ? "nd" : (k == 3 ?
	    "rd" : "th"));
	p1put_comment (buf);

	for( ; (k&1)==0 && k>2 ; k>>=1 )
	{
		p = mkexpr (OPCOMMA, p, putsteq(t1, t1));
	}

	if(k == 2) {

/* Write the power computation out immediately */
		putout (p);
		p = putx( mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1)));
	} else {
		t2 = Mktemp(type, PNULL);
		p = mkexpr (OPCOMMA, p, putassign(cpexpr(t2), cpexpr(t1)));

		for(k>>=1 ; k>1 ; k>>=1)
		{
			p = mkexpr (OPCOMMA, p, putsteq(t1, t1));
			if(k & 1)
			{
				p = mkexpr (OPCOMMA, p, putsteq(t2, t1));
			}
		}
/* Write the power computation out immediately */
		putout (p);
		p = putx( mkexpr(OPSTAR, cpexpr(t2),
		    mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1))));
	}
	frexpr(t1);
	if(t2)
		frexpr(t2);
	return p;
}




LOCAL Addrp intdouble(p)
Addrp p;
{
	register Addrp t;

	t = Mktemp(TYDREAL, PNULL);
	putout (putassign(cpexpr(t), p));
	return(t);
}





/* Complex-type variable assignment */

LOCAL Addrp putcxeq(p)
register expptr p;
{
	register Addrp lp, rp;
	expptr code;

	if(p->tag != TEXPR)
		badtag("putcxeq", p->tag);

	lp = putcx1(p->exprblock.leftp);
	rp = putcx1(p->exprblock.rightp);
	code = putassign ( realpart(lp), realpart(rp));

	if( ISCOMPLEX(p->exprblock.vtype) )
	{
		code = mkexpr (OPCOMMA, code, putassign
			(imagpart(lp), imagpart(rp)));
	}
	putout (code);
	frexpr(rp);
	free ((charptr) p);
	return lp;
}



/* putcxop -- used to write out embedded calls to complex functions, and
   complex arguments to procedures */

expptr putcxop(p)
expptr p;
{
	return (expptr)putaddr(putcx1(p));
}

#define PAIR(x,y) mkexpr (OPCOMMA, (x), (y))

LOCAL Addrp putcx1(p)
register expptr p;
{
	expptr q;
	Addrp lp, rp;
	register Addrp resp;
	int opcode;
	int ltype, rtype;
	long ts;
	expptr mkrealcon();

	if(p == NULL)
		return(NULL);

	switch(p->tag)
	{
	case TCONST:
		if( ISCOMPLEX(p->constblock.vtype) )
			p = (expptr) putconst(p);
		return( (Addrp) p );

	case TADDR:
		resp = &p->addrblock;
		if (addressable(p))
			return (Addrp) p;
		if ((q = resp->memoffset) && resp->isarray
					  && resp->vtype != TYCHAR) {
			if (ONEOF(resp->vstg, M(STGCOMMON)|M(STGEQUIV))
					&& resp->uname_tag == UNAM_NAME)
				q = mkexpr(OPMINUS, q,
					mkintcon(resp->user.name->voffset));
			ts = typesize[resp->vtype]
					* (resp->field ? 2 : 1);
			q = resp->memoffset = mkexpr(OPSLASH, q, ICON(ts));
			}
		else
			ts = 0;
		resp = Mktemp(tyint, PNULL);
		putout(putassign(cpexpr(resp), q));
		p->addrblock.memoffset = (expptr)resp;
		if (ts) {
			resp = &p->addrblock;
			q = mkexpr(OPSTAR, resp->memoffset, ICON(ts));
			if (ONEOF(resp->vstg, M(STGCOMMON)|M(STGEQUIV))
				&& resp->uname_tag == UNAM_NAME)
				q = mkexpr(OPPLUS, q,
				    mkintcon(resp->user.name->voffset));
			resp->memoffset = q;
			}
		return (Addrp) p;

	case TEXPR:
		if( ISCOMPLEX(p->exprblock.vtype) )
			break;
		resp = Mktemp(TYDREAL, NO);
		putout (putassign( cpexpr(resp), p));
		return(resp);

	default:
		badtag("putcx1", p->tag);
	}

	opcode = p->exprblock.opcode;
	if(opcode==OPCALL || opcode==OPCCALL)
	{
		Addrp t;
		p = putcall(p, &t);
		putout(p);
		return t;
	}
	else if(opcode == OPASSIGN)
	{
		return putcxeq (p);
	}

/* BUG  (inefficient)  Generates too many temporary variables */

	resp = Mktemp(p->exprblock.vtype, PNULL, p->exprblock.vtype==TYCOMPLEX);
	if(lp = putcx1(p->exprblock.leftp) )
		ltype = lp->vtype;
	if(rp = putcx1(p->exprblock.rightp) )
		rtype = rp->vtype;

	switch(opcode)
	{
	case OPCOMMA:
		frexpr(resp);
		resp = rp;
		rp = NULL;
		break;

	case OPNEG:
	case OPNEG1:
		putout (PAIR (
		putassign( realpart(resp), mkexpr(OPNEG, realpart(lp), ENULL)),
		putassign( imagpart(resp), mkexpr(OPNEG, imagpart(lp), ENULL))));
		break;

	case OPPLUS:
	case OPMINUS: { expptr r;
		r = putassign( realpart(resp),
		    mkexpr(opcode, realpart(lp), realpart(rp) ));
		if(rtype < TYCOMPLEX)
			q = putassign( imagpart(resp), imagpart(lp) );
		else if(ltype < TYCOMPLEX)
		{
			if(opcode == OPPLUS)
				q = putassign( imagpart(resp), imagpart(rp) );
			else
				q = putassign( imagpart(resp),
				    mkexpr(OPNEG, imagpart(rp), ENULL) );
		}
		else
			q = putassign( imagpart(resp),
			    mkexpr(opcode, imagpart(lp), imagpart(rp) ));
		r = PAIR (r, q);
		putout (r);
		break;
	    } /* case OPPLUS, OPMINUS: */
	case OPSTAR:
		if(ltype < TYCOMPLEX)
		{
			if( ISINT(ltype) )
				lp = intdouble(lp);
			putout (PAIR (
				putassign( realpart(resp),
				    mkexpr(OPSTAR, cpexpr(lp), realpart(rp))),
				putassign( imagpart(resp),
				    mkexpr(OPSTAR, cpexpr(lp), imagpart(rp)))));
		}
		else if(rtype < TYCOMPLEX)
		{
			if( ISINT(rtype) )
				rp = intdouble(rp);
			putout (PAIR (
				putassign( realpart(resp),
				    mkexpr(OPSTAR, cpexpr(rp), realpart(lp))),
				putassign( imagpart(resp),
				    mkexpr(OPSTAR, cpexpr(rp), imagpart(lp)))));
		}
		else	{
			putout (PAIR (
				putassign( realpart(resp), mkexpr(OPMINUS,
				    mkexpr(OPSTAR, realpart(lp), realpart(rp)),
				    mkexpr(OPSTAR, imagpart(lp), imagpart(rp)))),
				putassign( imagpart(resp), mkexpr(OPPLUS,
				    mkexpr(OPSTAR, realpart(lp), imagpart(rp)),
				    mkexpr(OPSTAR, imagpart(lp), realpart(rp))))));
		}
		break;

	case OPSLASH:
		/* fixexpr has already replaced all divisions
		 * by a complex by a function call
		 */
		if( ISINT(rtype) )
			rp = intdouble(rp);
		putout (PAIR (
			putassign( realpart(resp),
			    mkexpr(OPSLASH, realpart(lp), cpexpr(rp))),
			putassign( imagpart(resp),
			    mkexpr(OPSLASH, imagpart(lp), cpexpr(rp)))));
		break;

	case OPCONV:
		if( ISCOMPLEX(lp->vtype) )
			q = imagpart(lp);
		else if(rp != NULL)
			q = (expptr) realpart(rp);
		else
			q = mkrealcon(TYDREAL, "0");
		putout (PAIR (
			putassign( realpart(resp), realpart(lp)),
			putassign( imagpart(resp), q)));
		break;

	default:
		badop("putcx1", opcode);
	}

	frexpr(lp);
	frexpr(rp);
	free( (charptr) p );
	return(resp);
}




/* Only .EQ. and .NE. may be performed on COMPLEX data, other relations
   are not defined */

LOCAL expptr putcxcmp(p)
register expptr p;
{
	int opcode;
	register Addrp lp, rp;
	expptr q;

	if(p->tag != TEXPR)
		badtag("putcxcmp", p->tag);

	opcode = p->exprblock.opcode;
	lp = putcx1(p->exprblock.leftp);
	rp = putcx1(p->exprblock.rightp);

	q = mkexpr( opcode==OPEQ ? OPAND : OPOR ,
	    mkexpr(opcode, realpart(lp), realpart(rp)),
	    mkexpr(opcode, imagpart(lp), imagpart(rp)) );

	free( (charptr) lp);
	free( (charptr) rp);
	free( (charptr) p );
	return 	putx( fixexpr(q) );
}

/* putch1 -- Forces constants into the literal pool, among other things */

LOCAL Addrp putch1(p)
register expptr p;
{
	Addrp t;
	expptr e;

	switch(p->tag)
	{
	case TCONST:
		return( putconst(p) );

	case TADDR:
		return( (Addrp) p );

	case TEXPR:
		switch(p->exprblock.opcode)
		{
			expptr q;

		case OPCALL:
		case OPCCALL:

			p = putcall(p, &t);
			putout (p);
			break;

		case OPCONCAT:
			t = Mktemp(TYCHAR, ICON(lencat(p)));
			q = (expptr) cpexpr(p->headblock.vleng);
			p = putcat( cpexpr(t), p );
			/* put the correct length on the block */
			frexpr(t->vleng);
			t->vleng = q;
			putout (p);
			break;

		case OPCONV:
			if(!ISICON(p->exprblock.vleng)
			    || p->exprblock.vleng->constblock.Const.ci!=1
			    || ! INT(p->exprblock.leftp->headblock.vtype) )
				Fatal("putch1: bad character conversion");
			t = Mktemp(TYCHAR, ICON(1));
			e = mkexpr(OPCONV, t, ENULL);
			e->headblock.vtype = tyint;
			p = putop( mkexpr(OPASSIGN, cpexpr(e), p));
			putout (p);
			break;
		default:
			badop("putch1", p->exprblock.opcode);
		}
		return(t);

	default:
		badtag("putch1", p->tag);
	}
	/* NOTREACHED */ return 0;
}


/* putchop -- Write out a character actual parameter; that is, this is
   part of a procedure invocation */

LOCAL Addrp putchop(p)
expptr p;
{
	p = putaddr(putch1(p));
	return (Addrp)p;
}




LOCAL expptr putcheq(p)
register expptr p;
{
	expptr lp, rp;

	if(p->tag != TEXPR)
		badtag("putcheq", p->tag);

	lp = p->exprblock.leftp;
	rp = p->exprblock.rightp;
	frexpr(p->exprblock.vleng);
	free( (charptr) p );

/* If s = t // u, don't bother copying the result, write it directly into
   this buffer */

	if( rp->tag==TEXPR && rp->exprblock.opcode==OPCONCAT )
		p = putcat(lp, rp);
	else if( ISONE(lp->headblock.vleng) && ISONE(rp->headblock.vleng) ) {
		lp = mkexpr(OPCONV, putx(lp), ENULL);
		rp = mkexpr(OPCONV, putx(rp), ENULL);
		lp->headblock.vtype = rp->headblock.vtype = tyint;
		p = putop(mkexpr(OPASSIGN, lp, rp));
		}
	else
		p = putx( call2(TYSUBR, "s_copy", lp, rp) );
	return p;
}




LOCAL expptr putchcmp(p)
register expptr p;
{
	expptr lp, rp;

	if(p->tag != TEXPR)
		badtag("putchcmp", p->tag);

	lp = p->exprblock.leftp;
	rp = p->exprblock.rightp;

	if(ISONE(lp->headblock.vleng) && ISONE(rp->headblock.vleng) ) {
		lp = mkexpr(OPCONV, putx(lp), ENULL);
		rp = mkexpr(OPCONV, putx(rp), ENULL);
		lp->headblock.vtype = rp->headblock.vtype = tyint;
		}
	else {
		lp = call2(TYINT,"s_cmp", lp, rp);
		rp = ICON(0);
		}
	p->exprblock.leftp = lp;
	p->exprblock.rightp = rp;
	p = putop(p);
	return p;
}





/* putcat -- Writes out a concatenation operation.  Two temporary arrays
   are allocated,   putct1()   is called to initialize them, and then a
   call to runtime library routine   s_cat()   is inserted.

	This routine generates code which will perform an  (nconc lhs rhs)
   at runtime.  The runtime funciton does not return a value, the routine
   that calls this   putcat   must remember the name of   lhs.
*/


LOCAL expptr putcat(lhs, rhs)
register Addrp lhs;
register expptr rhs;
{
	int n, tyi;
	Addrp length_var, string_var;
	expptr p;

/* Create the temporary arrays */

	n = ncat(rhs);
	length_var = mktmpn(n, tyioint, PNULL);
	string_var = mktmpn(n, TYADDR, PNULL);
	frtemp((Addrp)cpexpr(length_var));
	frtemp((Addrp)cpexpr(string_var));

/* Initialize the arrays */

	n = 0;
	p1put_comment ("Writing concatenation");
	putct1(rhs, length_var, string_var, &n);

/* Create the invocation */

	tyi = tyint;
	tyint = tyioint;	/* for -I2 */
	p = putx (call4 (TYSUBR, "s_cat", lhs, string_var, length_var,
		putconst(ICON(n))));
	tyint = tyi;

	return p;
}





LOCAL putct1(q, length_var, string_var, ip)
register expptr q;
register Addrp length_var, string_var;
int *ip;
{
	int i;
	Addrp length_copy, string_copy;
	extern int szleng;

	if(q->tag==TEXPR && q->exprblock.opcode==OPCONCAT)
	{
		putct1(q->exprblock.leftp, length_var, string_var,
		    ip);
		putct1(q->exprblock.rightp, length_var, string_var,
		    ip);
		frexpr (q -> exprblock.vleng);
		free ((charptr) q);
	}
	else
	{
		i = (*ip)++;
		length_copy = (Addrp) cpexpr(length_var);
		length_copy->memoffset =
		    mkexpr(OPPLUS,length_copy->memoffset, ICON(i*szleng));
		string_copy = (Addrp) cpexpr(string_var);
		string_copy->memoffset =
		    mkexpr(OPPLUS, string_copy->memoffset,
			ICON(i*typesize[TYLONG]));
		putout (PAIR (putassign (length_copy, cpexpr
			(q->headblock.vleng)),
			putassign (string_copy, addrof (putch1 (q)))));
	}
}

/* putaddr -- seems to write out function invocation actual parameters */

LOCAL expptr putaddr(p)
register Addrp p;
{
	if (!p)
		return ENULL;
	if( p->tag==TERROR || (p->memoffset!=NULL && ISERROR(p->memoffset)) )
	{
		frexpr(p);
		return ENULL;
	}
	if (p->isarray && p->memoffset)
		p->memoffset = putx(p->memoffset);
	return (expptr) p;
}

 LOCAL expptr
addrfix(e)		/* fudge character string length if it's a TADDR */
 expptr e;
{
	return e->tag == TADDR ? mkexpr(OPIDENTITY, e, ENULL) : e;
	}

 LOCAL int
typekludge(ccall, q, at, j)
 int ccall;
 register expptr q;
 Atype *at;
 int j;	/* alternate type */
{
	register int i, k;
	extern int iocalladdr;
	register Namep np;

	/* Return value classes:
	 *	< 100 ==> Fortran arg (pointer to type)
	 *	< 200 ==> C arg
	 *	< 300 ==> procedure arg
	 *	< 400 ==> external, no explicit type
	 *	< 500 ==> arg that may turn out to be
	 *		  either a variable or a procedure
	 */

	k = q->headblock.vtype;
	if (ccall) {
		if (k == TYREAL)
			k = TYDREAL;	/* force double for library routines */
		return k + 100;
		}
	if (k == TYADDR)
		return iocalladdr;
	i = q->tag;
	if ((i == TEXPR)
	||  (i == TADDR && q->addrblock.charleng)
	||   i == TCONST)
		k = TYFTNLEN + 100;
	else if (i == TADDR)
	    switch(q->addrblock.vclass) {
		case CLPROC:
			if (q->addrblock.uname_tag != UNAM_NAME)
				k += 200;
			else if ((np = q->addrblock.user.name)->vprocclass
					!= PTHISPROC) {
				if (k && !np->vimpltype)
					k += 200;
				else {
					if (j > 200 && infertypes && j < 300) {
						k = j;
						inferdcl(np, j-200);
						}
					else k = (np->vstg == STGEXT
						? extsymtab[np->vardesc.varno].extype
						: 0) + 200;
					at->cp = mkchain((char *)np, at->cp);
					}
				}
			else if (k == TYSUBR)
				k += 200;
			break;

		case CLUNKNOWN:
			if (q->addrblock.vstg == STGARG
			 && q->addrblock.uname_tag == UNAM_NAME) {
				k += 400;
				at->cp = mkchain((char *)q->addrblock.user.name,
						at->cp);
				}
		}
	else if (i == TNAME && q->nameblock.vstg == STGARG) {
		np = &q->nameblock;
		switch(np->vclass) {
		    case CLPROC:
			if (!np->vimpltype)
				k += 200;
			else if (j <= 200 || !infertypes || j >= 300)
				k += 300;
			else {
				k = j;
				inferdcl(np, j-200);
				}
			goto add2chain;

		    case CLUNKNOWN:
			/* argument may be a scalar variable or a function */
			if (np->vimpltype && j && infertypes
			&& j < 300) {
				inferdcl(np, j % 100);
				k = j;
				}
			else
				k += 400;

			/* to handle procedure args only so far known to be
			 * external, save a pointer to the symbol table entry...
		 	 */
 add2chain:
			at->cp = mkchain((char *)np, at->cp);
		    }
		}
	return k;
	}

 char *
Argtype(k, buf)
 int k;
 char *buf;
{
	if (k < 100) {
		sprintf(buf, "%s variable", ftn_types[k]);
		return buf;
		}
	if (k < 200) {
		k -= 100;
		return k == TYFTNLEN ? "ftnlen" : ftn_types[k];
		}
	if (k < 300) {
		k -= 200;
		if (k == TYSUBR)
			return ftn_types[TYSUBR];
		sprintf(buf, "%s function", ftn_types[k]);
		return buf;
		}
	if (k < 400)
		return "external argument";
	k -= 400;
	sprintf(buf, "%s argument", ftn_types[k]);
	return buf;
	}

 static void
atype_squawk(at, msg)
 Argtypes *at;
 char *msg;
{
	register Atype *a, *ae;
	warn(msg);
	for(a = at->atypes, ae = a + at->nargs; a < ae; a++)
		frchain(&a->cp);
	at->nargs = -1;
	if (at->changes & 2)
		proc_protochanges++;
	}

 static char inconsist[] = "inconsistent calling sequences for ";

 void
bad_atypes(at, fname, i, j, k, here, prev)
 Argtypes *at;
 char *fname, *here, *prev;
 int i, j, k;
{
	char buf[208], buf1[32], buf2[32];

	sprintf(buf, "%s%.90s,\n\targ %d: %s%s%s %s.",
		inconsist, fname, i, here, Argtype(k, buf1),
		prev, Argtype(j, buf2));
	atype_squawk(at, buf);
	}

 int
type_fixup(at,a,k)
 Argtypes *at;
 Atype *a;
 int k;
{
	register struct Entrypoint *ep;
	if (!infertypes)
		return 0;
	for(ep = entries; ep; ep = ep->entnextp)
		if (at == ep->entryname->arginfo) {
			a->type = k % 100;
			return proc_argchanges = 1;
			}
	return 0;
	}


 void
save_argtypes(arglist, at0, at1, ccall, fname, stg, nchargs, type)
 chainp arglist;
 Argtypes **at0, **at1;
 int ccall, stg, nchargs, type;
 char *fname;
{
	Argtypes *at;
	chainp cp;
	int i, i0, j, k, nargs, *t, *te;
	Atype *atypes;
	expptr q;
	char buf[208];
	static int initargs[4] = {TYCOMPLEX, TYDCOMPLEX, TYCHAR, TYFTNLEN+100};
	static int *init_ap[TYSUBR+1] = {0,0,0,0,0,0,
				initargs, initargs+1,0,initargs+2};
	static int init_ac[TYSUBR+1] = { 0,0,0,0,0,0,
				1, 1, 0, 2};

	i = i0 = init_ac[type];
	t = init_ap[type];
	te = t + i;
	if (at = *at0) {
		*at1 = at;
		nargs = at->nargs;
		if (nargs < 0) { /* inconsistent usage seen */
			if (type) {
				if (at->changes & 2)
					--proc_protochanges;
				goto newlist;
				}
			return;
			}
		for(cp = arglist; cp; cp = cp->nextp)
			i++;
		if ((i += nchargs) != nargs) {
			sprintf(buf,
		"%s%.90s:\n\there %d, previously %d args and string lengths.",
				inconsist, fname, i, nargs);
			atype_squawk(at, buf);
 retn:
			if (type)
				goto newlist;
			return;
			}
		atypes = at->atypes;
		i = 0;
		for(; t < te; atypes++) {
			i++;
			j = atypes->type;
			k = *t++;
			if (j != k)
				goto badtypes;
			}
		for(cp = arglist; cp; atypes++, cp = cp->nextp) {
			++i;
			j = atypes->type;
			if (!(q = (expptr)cp->datap))
				continue;
			k = typekludge(ccall, q, atypes, j);
			if (k >= 300 || k == j)
				continue;
			if (j >= 300) {
				if (k >= 200) {
					if (k == TYUNKNOWN + 200)
						continue;
					if (j % 100 != k - 200
					 && k != TYSUBR + 200
					 && j != TYUNKNOWN + 300
					 && !type_fixup(at,atypes,k))
						goto badtypes;
					}
				else if (j % 100 % TYSUBR != k % TYSUBR
						&& !type_fixup(at,atypes,k))
					goto badtypes;
				}
			else if (k < 200 || j < 200)
				if (j)
					goto badtypes;
				else ; /* fall through to update */
			else if (k == TYUNKNOWN+200)
				continue;
			else if (j != TYUNKNOWN+200)
				{
 badtypes:
				bad_atypes(at, fname, i, j, k, "here ",
						", previously");
				if (type) {
					/* we're defining the procedure */
					t = init_ap[type];
					te = t + i0;
					proc_argchanges = 1;
					goto newlist;
					}
				goto retn;
				}
			/* We've subsequently learned the right type,
			   as in the call on zoo below...

				subroutine foo(x, zap)
				external zap
				call goo(zap)
				x = zap(3)
				call zoo(zap)
				end
			 */
			atypes->type = k;
			at->changes |= 1;
			}
		if (type)
			at->changes = 0;
		return;
		}
 newlist:
	i = i0 + nchargs;
	for(cp = arglist; cp; cp = cp->nextp)
		i++;
	k = sizeof(Argtypes) + (i-1)*sizeof(Atype);
	*at0 = *at1 = at = stg == STGEXT ? (Argtypes *)gmem(k,1)
					 : (Argtypes *) mem(k,1);
	at->nargs = i;
	at->changes = 0;
	atypes = at->atypes;
	for(; t < te; atypes++) {
		atypes->type = *t++;
		atypes->cp = 0;
		}
	for(cp = arglist; cp; atypes++, cp = cp->nextp) {
		atypes->cp = 0;
		atypes->type = (q = (expptr)cp->datap)
			? typekludge(ccall, q, atypes, 0)
			: 0;
		}
	for(; --nchargs >= 0; atypes++) {
		atypes->type = TYFTNLEN + 100;
		atypes->cp = 0;
		}
	}

 void
saveargtypes(p)		/* for writing prototypes */
 register Exprp p;
{
	Addrp a;
	Argtypes **at0, **at1;
	Namep np;
	chainp arglist;
	expptr rp;
	struct Extsym *e;
	char *fname;

	a = (Addrp)p->leftp;
	switch(a->vstg) {
		case STGEXT:
			switch(a->uname_tag) {
				case UNAM_EXTERN:	/* e.g., sqrt() */
					e = extsymtab + a->memno;
					at0 = at1 = &e->arginfo;
					fname = e->fextname;
					break;
				case UNAM_NAME:
					np = a->user.name;
					at0 = &extsymtab[np->vardesc.varno].arginfo;
					at1 = &np->arginfo;
					fname = np->fvarname;
					break;
				default:
					goto bug;
				}
			break;
		case STGARG:
			if (a->uname_tag != UNAM_NAME)
				goto bug;
			np = a->user.name;
			at0 = at1 = &np->arginfo;
			fname = np->fvarname;
			break;
		default:
	 bug:
			Fatal("Confusion in saveargtypes");
		}
	rp = p->rightp;
	arglist = rp && rp->tag == TLIST ? rp->listblock.listp : 0;
	save_argtypes(arglist, at0, at1, p->opcode == OPCCALL,
		fname, a->vstg, 0, 0);
	}

/* putcall - fix up the argument list, and write out the invocation.   p
   is expected to be initialized and point to an OPCALL or OPCCALL
   expression.  The return value is a pointer to a temporary holding the
   result of a COMPLEX or CHARACTER operation, or NULL. */

LOCAL expptr putcall(p, temp)
register Exprp p;
Addrp *temp;
{
    chainp arglist;		/* Pointer to actual arguments, if any */
    chainp charsp;		/* List of copies of the variables which
				   hold the lengths of character
				   parameters (other than procedure
				   parameters) */
    chainp cp;			/* Iterator over argument lists */
    register expptr q;		/* Pointer to the current argument */
    Addrp fval;			/* Function return value */
    int type;			/* type of the call - presumably this was
				   set elsewhere */
    int byvalue;		/* True iff we don't want to massage the
				   parameter list, since we're calling a C
				   library routine */
    extern int Castargs;
    char *s;

    type = p -> vtype;
    charsp = NULL;
    byvalue =  (p->opcode == OPCCALL);

/* Verify the actual parameters */

    if (p == (Exprp) NULL)
	err ("putcall:  NULL call expression");
    else if (p -> tag != TEXPR)
	erri ("putcall:  expected TEXPR, got '%d'", p -> tag);

/* Find the argument list */

    if(p->rightp && p -> rightp -> tag == TLIST)
	arglist = p->rightp->listblock.listp;
    else
	arglist = NULL;

/* Count the number of explicit arguments, including lengths of character
   variables */

    for(cp = arglist ; cp ; cp = cp->nextp)
	if(!byvalue) {
	    q = (expptr) cp->datap;
	    if( ISCONST(q) )
	    {

/* Even constants are passed by reference, so we need to put them in the
   literal table */

		q = (expptr) putconst(q);
		cp->datap = (char *) q;
	    }

/* Save the length expression of character variables (NOT character
   procedures) for the end of the argument list */

	    if( ISCHAR(q) &&
		(q->headblock.vclass != CLPROC
		|| q->headblock.vstg == STGARG
			&& q->tag == TADDR
			&& q->addrblock.uname_tag == UNAM_NAME
			&& q->addrblock.user.name->vprocclass == PTHISPROC))
	    {
		charsp = mkchain((char *)cpexpr(q->headblock.vleng), charsp);
		if (q->headblock.vclass == CLUNKNOWN
		 && q->headblock.vstg == STGARG)
			q->addrblock.user.name->vpassed = 1;
	    }
	}
    charsp = revchain(charsp);

/* If the routine is a CHARACTER function ... */

    if(type == TYCHAR)
    {
	if( ISICON(p->vleng) )
	{

/* Allocate a temporary to hold the return value of the function */

	    fval = Mktemp(TYCHAR, p->vleng);
	}
	else    {
		err("adjustable character function");
		if (temp)
			*temp = 0;
		return 0;
		}
    }

/* If the routine is a COMPLEX function ... */

    else if( ISCOMPLEX(type) )
	fval = Mktemp(type, PNULL);
    else
	fval = NULL;

/* Write the function name, without taking its address */

    p -> leftp = putx(fixtype(putaddr(p->leftp)));

    if(fval)
    {
	chainp prepend;

/* Prepend a copy of the function return value buffer out as the first
   argument. */

	prepend = mkchain((char *)putx(putaddr(cpexpr(fval))), arglist);

/* If it's a character function, also prepend the length of the result */

	if(type==TYCHAR)
	{

	    prepend->nextp = mkchain((char *)putx(mkconv(TYLENG,
					p->vleng)), arglist);
	}
	p -> rightp -> listblock.listp = prepend;
    }

/* Scan through the fortran argument list */

    for(cp = arglist ; cp ; cp = cp->nextp)
    {
	q = (expptr) (cp->datap);
	if (q == ENULL)
	    err ("putcall:  NULL argument");

/* call putaddr only when we've got a parameter for a C routine or a
   memory resident parameter */

	if (q -> tag == TCONST && !byvalue)
	    q = (expptr) putconst (q);

	if(q->tag==TADDR && (byvalue || q->addrblock.vstg!=STGREG) )
		cp->datap = (char *)putx(fixtype(putaddr(q)));
	else if( ISCOMPLEX(q->headblock.vtype) )
	    cp -> datap = (char *) putx (fixtype (putcxop(q)));
	else if (ISCHAR(q) )
	    cp -> datap = (char *) putx (fixtype (putchop(q)));
	else if( ! ISERROR(q) )
	{
	    if(byvalue
	    || q->tag == TEXPR && q->exprblock.opcode == OPCHARCAST)
		cp -> datap = (char *) putx(q);
	    else {
		Addrp t;

/* If we've got a register parameter, or (maybe?) a constant, save it in a
   temporary first */

		t = Mktemp(q->headblock.vtype, q->headblock.vleng);

/* Assign to temporary variables before invoking the subroutine or
   function */

		putout (putassign( cpexpr(t), q ));
		cp -> datap = (char *) t;
	    } /* else */
	} /* if !ISERROR(q) */
    }

/* Now adjust the lengths of the CHARACTER parameters */

    for(cp = charsp ; cp ; cp = cp->nextp)
	cp->datap = (char *)addrfix(putx(
			/* in case MAIN has a character*(*)... */
			(s = cp->datap) ? mkconv(TYLENG,(expptr)s)
					 : ICON(0)));

/* ... and add them to the end of the argument list */

    hookup (arglist, charsp);

/* Return the name of the temporary used to hold the results, if any was
   necessary. */

    if (temp) *temp = fval;
    else frexpr (fval);

    saveargtypes(p);

    return (expptr) p;
}



/* putmnmx -- Put min or max.   p   must point to an EXPR, not just a
   CONST */

LOCAL expptr putmnmx(p)
register expptr p;
{
	int op, op2, type;
	expptr qp;
	chainp p0, p1;
	Addrp sp, tp;
	char comment_buf[80];

	if(p->tag != TEXPR)
		badtag("putmnmx", p->tag);

	type = p->exprblock.vtype;
	op = p->exprblock.opcode;
	op2 = op == OPMIN ? OPMIN2 : OPMAX2;
	p0 = p->exprblock.leftp->listblock.listp;
	free( (charptr) (p->exprblock.leftp) );
	free( (charptr) p );

	/* special case for two addressable operands */

	if (addressable((expptr)p0->datap)
	 && (p1 = p0->nextp)
	 && addressable((expptr)p1->datap)
	 && !p1->nextp) {
		if (type == TYREAL && forcedouble)
			op2 = op == OPMIN ? OPDMIN : OPDMAX;
		p = mkexpr(op2, mkconv(type, cpexpr((expptr)p0->datap)),
				mkconv(type, cpexpr((expptr)p1->datap)));
		frchain(&p0);
		return p;
		}

	/* general case */

	sp = Mktemp(type, PNULL);

/* We only need a second temporary if the arg list has an unaddressable
   value */

	tp = (Addrp) NULL;
	qp = ENULL;
	for (p1 = p0 -> nextp; p1; p1 = p1 -> nextp)
		if (!addressable ((expptr) p1 -> datap)) {
			tp = Mktemp(type, PNULL);
			qp = mkexpr(op2, cpexpr(tp), cpexpr(sp));
			qp = fixexpr(qp);
			break;
		} /* if */

/* Now output the appropriate number of assignments and comparisons.  Min
   and max are implemented by the simple O(n) algorithm:

	min (a, b, c, d) ==>
	{ <type> t1, t2;

	    t1 = a;
	    t2 = b; t1 = (t1 < t2) ? t1 : t2;
	    t2 = c; t1 = (t1 < t2) ? t1 : t2;
	    t2 = d; t1 = (t1 < t2) ? t1 : t2;
	}
*/

	sprintf (comment_buf, "Computing M%s", op == OPLT ? "IN" : "AX");
	p1put_comment (comment_buf);

	p = putassign (cpexpr(sp), (expptr)p0->datap);

	for(p1 = p0->nextp ; p1 ; p1 = p1->nextp)
	{
		expptr this_temp;

		if (addressable ((expptr) p1 -> datap)) {
			expptr this_arg =
				mkconv(type, cpexpr((expptr)p1->datap));
			this_temp = mkexpr(op2, this_arg, cpexpr(sp));
			this_temp = fixexpr(this_temp);
		} else {
			this_temp = (expptr) cpexpr (qp);
			p = mkexpr(OPCOMMA, p,
				putassign(cpexpr(tp), (expptr)p1->datap));
		} /* else */

		if(p1->nextp)
			p = mkexpr(OPCOMMA, p, putassign (cpexpr(sp), this_temp));
		else {
			putout (p);
			if (type == TYREAL && forcedouble)
				this_temp->exprblock.opcode =
					op == OPMIN ? OPDMIN : OPDMAX;
			p = putx(this_temp);
			if (qp)
				frexpr (qp);
		} /* else */
	} /* for */

	frchain( &p0 );
	return p;
}




/* simoffset -- Simply offset expression.  This routine does some limited
   pattern matching to simplify an expression describing the offset value.
   In particular, only two kinds of additions are extracted from the input
   expression.  Noninteger expressions return a value of 0.  Below are some
   sample reductions:

	Input Expression	Output Expression	Return value
----------------------------------------------------------------------------
	(1 + 5) * 4	   |	1 * 4			20
	(x + 5) * 4	   |	x * 4			20
	1 + 5		   |	null			6
	x + 5		   |	x			5
	1		   |	null			1
	4 * (1 + 5)	   |	4 * (1 + 5)		0
	5 + x		   |	5 + x			0
	1 * 5		   |	1 * 5			0
	1.0		   |	1.0			0
	null		   |	null			0
----------------------------------------------------------------------------

 LOCAL declaration added 10 June 88 by mwm */

LOCAL ftnint simoffset(p0)
expptr *p0;
{
	ftnint offset;		/* Computed value, to be returned */
	register expptr p, lp, rp;

	offset = 0;
	p = *p0;
	if(p == NULL)
		return(0);

/* non-integer offsets are mapped onto 0 */

	if( ! ISINT(p->headblock.vtype) )
		return(0);

/* Simplify expressions matching one instance of the distributive law:

	if P == (x + CONST1) * CONST2 then
		set P = (x * CONST2) + (actual-product-of CONST1*CONST2)
*/

	if(p->tag==TEXPR && p->exprblock.opcode==OPSTAR)
	{
		ftnint prod;

		lp = p->exprblock.leftp;
		rp = p->exprblock.rightp;
		if(ISICON(rp) && lp->tag==TEXPR &&
		    lp->exprblock.opcode==OPPLUS && ISICON(lp->exprblock.rightp))
		{
			p->exprblock.opcode = OPPLUS;
			lp->exprblock.opcode = OPSTAR;
			prod = rp->constblock.Const.ci *
			    lp->exprblock.rightp->constblock.Const.ci;
			lp->exprblock.rightp->constblock.Const.ci =
			    rp->constblock.Const.ci;
			rp->constblock.Const.ci = prod;
		} /* if (ISICON(rp) ... */
	} /* if (p->tag==TEXPR ... */

/* Add the constant part of an addition to the   offset   return value.  That is,

	if P == (x + CONST) then
		set P = x
		set offset += CONST
*/

	if(p->tag==TEXPR && p->exprblock.opcode==OPPLUS &&
	    ISICON(p->exprblock.rightp))
	{
		rp = p->exprblock.rightp;
		lp = p->exprblock.leftp;
		offset += rp->constblock.Const.ci;
		frexpr(rp);
		free( (charptr) p );
		*p0 = lp;
	}

/* If all that remains in the expression (after the above reductions) is a
   constant, add it to   offset   */

	else if( ISCONST(p) )
	{
		offset += p->constblock.Const.ci;
		frexpr(p);
		*p0 = NULL;
	}

	return(offset);
}
