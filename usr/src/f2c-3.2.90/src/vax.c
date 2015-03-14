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

#include "defs.h"
#include "pccdefs.h"
#include "output.h"

int regnum[] =  {
	11, 10, 9, 8, 7, 6 };

/* Put out a constant integer */

prconi(fp, n)
FILEP fp;
ftnint n;
{
	fprintf(fp, "\t%ld\n", n);
}



/* Put out a constant address */

prcona(fp, a)
FILEP fp;
ftnint a;
{
	fprintf(fp, "\tL%ld\n", a);
}



prconr(fp, x, k)
 FILEP fp;
 int k;
 Constp x;
{
	char *x0, *x1;
	char cdsbuf0[64], cdsbuf1[64];

	if (k > 1) {
		if (x->vstg) {
			x0 = x->Const.cds[0];
			x1 = x->Const.cds[1];
			}
		else {
			x0 = cds(dtos(x->Const.cd[0]), cdsbuf0);
			x1 = cds(dtos(x->Const.cd[1]), cdsbuf1);
			}
		fprintf(fp, "\t%s %s\n", x0, x1);
		}
	else
		fprintf(fp, "\t%s\n", x->vstg ? x->Const.cds[0]
				: cds(dtos(x->Const.cd[0]), cdsbuf0));
}


char *memname(stg, mem)
 int stg;
 long mem;
{
	static char s[20];

	switch(stg)
	{
	case STGCOMMON:
	case STGEXT:
		sprintf(s, "_%s", extsymtab[mem].cextname);
		break;

	case STGBSS:
	case STGINIT:
		sprintf(s, "v.%ld", mem);
		break;

	case STGCONST:
		sprintf(s, "L%ld", mem);
		break;

	case STGEQUIV:
		sprintf(s, "q.%ld", mem+eqvstart);
		break;

	default:
		badstg("memname", stg);
	}
	return(s);
}

/* make_int_expr -- takes an arbitrary expression, and replaces all
   occurrences of arguments with indirection */

expptr make_int_expr (e)
expptr e;
{
    if (e != ENULL)
	switch (e -> tag) {
	    case TADDR:
	        if (e -> addrblock.vstg == STGARG)
		    e = mkexpr (OPWHATSIN, e, ENULL);
	        break;
	    case TEXPR:
	        e -> exprblock.leftp = make_int_expr (e -> exprblock.leftp);
	        e -> exprblock.rightp = make_int_expr (e -> exprblock.rightp);
	        break;
	    default:
	        break;
	} /* switch */

    return e;
} /* make_int_expr */



/* prune_left_conv -- used in prolog() to strip type cast away from
   left-hand side of parameter adjustments.  This is necessary to avoid
   error messages from cktype() */

expptr prune_left_conv (e)
expptr e;
{
    struct Exprblock *leftp;

    if (e && e -> tag == TEXPR && e -> exprblock.leftp &&
	    e -> exprblock.leftp -> tag == TEXPR) {
	leftp = &(e -> exprblock.leftp -> exprblock);
	if (leftp -> opcode == OPCONV) {
	    e -> exprblock.leftp = leftp -> leftp;
	    free ((charptr) leftp);
	}
    }

    return e;
} /* prune_left_conv */


#define CHECK_COMMENT if (!wrote_comment) { wrote_comment = 1; \
		nice_printf (outfile, "/* Parameter adjustments */\n"); }

prolog(outfile, ep)
FILE *outfile;
struct Entrypoint *ep;
{
	int i, nd;
	int size;
	register chainp p;
	register Namep q;
	register struct Dimblock *dp;
	int wrote_comment = 0;

/* Give this location the name of the current block */

	if(procclass == CLMAIN) {
		if(fudgelabel)
		{
			fudgelabel = 0;
			fixlwm();
		}
	} else if(ep->entryname)
		if(fudgelabel)
		{
			fudgelabel = 0;
			fixlwm();
		}

	if(procclass == CLBLOCK)
		return;

/* Compute the base addresses and offsets for the array parameters, and
   assign these values to local variables */

	for(p = ep->arglist ; p ; p = p->nextp)
	{
	    q = (Namep) p->datap;
	    if(dp = q->vdim)	/* if this param is an array ... */
	    {
		expptr Q, expr;
		nd = dp->ndim - 1;
		for(i = 0 ; i <= nd; ++i)

/* Store the variable length of each dimension (which is fixed upon
   runtime procedure entry) into a local variable */

		    if ((Q = dp->dims[i].dimexpr)
			&& (i < nd || !q->vlastdim)) {
			expr = (expptr)cpexpr(Q);
			CHECK_COMMENT;
			output_and_free_statement (outfile, mkexpr (OPASSIGN,
				fixtype(cpexpr(dp->dims[i].dimsize)), expr));
		    } /* if dp -> dims[i].dimexpr */

/* size   will equal the size of a single element, or -1 if the type is
   variable length character type */

		size = typesize[ q->vtype ];
		if(q->vtype == TYCHAR)
		    if( ISICON(q->vleng) )
			size *= q->vleng->constblock.Const.ci;
		    else
			size = -1;

		/* Fudge the argument pointers for arrays so subscripts
		 * are 0-based. Not done if array bounds are being checked.
		 */
		if(dp->basexpr) {

/* Compute the base offset for this procedure */

		    CHECK_COMMENT;
		    output_and_free_statement (outfile, mkexpr (OPASSIGN,
			    cpexpr(fixtype(dp->baseoffset)),
			    cpexpr(fixtype(dp->basexpr))));
		} /* if dp -> basexpr */

		if(! checksubs) {
		    if(dp->basexpr) {
			expptr tp;

/* If the base of this array has a variable adjustment ... */

			tp = (expptr) cpexpr (dp -> baseoffset);
			if(size < 0 || q -> vtype == TYCHAR)
			    tp = mkexpr (OPSTAR, tp, cpexpr (q -> vleng));

			CHECK_COMMENT;
			tp = mkexpr (OPMINUSEQ,
				mkconv (TYADDR, (expptr)p->datap),
				mkconv(TYINT, fixtype
				(fixtype (tp))));
/* Avoid type clash by removing the type conversion */
			tp = prune_left_conv (tp);
			output_and_free_statement (outfile, tp);
		    } else if(dp->baseoffset->constblock.Const.ci != 0) {

/* if the base of this array has a nonzero constant adjustment ... */

			expptr tp;

			CHECK_COMMENT;
			if(size > 0 && q -> vtype != TYCHAR) {
			    tp = prune_left_conv (mkexpr (OPMINUSEQ,
				    mkconv (TYADDR, (expptr)p->datap),
				    mkconv (TYINT, fixtype
				    (cpexpr (dp->baseoffset)))));
			    output_and_free_statement (outfile, tp);
			} else {
			    tp = prune_left_conv (mkexpr (OPMINUSEQ,
				    mkconv (TYADDR, (expptr)p->datap),
				    mkconv (TYINT, fixtype
				    (mkexpr (OPSTAR, cpexpr (dp -> baseoffset),
				    cpexpr (q -> vleng))))));
			    output_and_free_statement (outfile, tp);
			} /* else */
		    } /* if dp -> baseoffset -> const */
		} /* if !checksubs */
	    }
	}
	if (wrote_comment)
	    nice_printf (outfile, "\n/* Function Body */\n");
} /* prolog */

#undef CHECK_COMMENT

fixlwm()
{
	extern lwmno;
	lwmno = procno;
}
