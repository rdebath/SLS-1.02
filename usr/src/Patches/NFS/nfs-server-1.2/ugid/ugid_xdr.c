#include <rpc/rpc.h>
#include "ugid.h"


bool_t
xdr_ugname(xdrs, objp)
	XDR *xdrs;
	ugname *objp;
{
	if (!xdr_string(xdrs, objp, MAXUGLEN)) {
		return (FALSE);
	}
	return (TRUE);
}


