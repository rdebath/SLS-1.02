#include <rpc/rpc.h>
#include <sys/time.h>
#include "ugid.h"

static struct timeval TIMEOUT = { 25, 0 };

int *
authenticate_1(argp, clnt)
	int *argp;
	CLIENT *clnt;
{
	static int res;

	bzero(&res, sizeof(res));
	if (clnt_call(clnt, AUTHENTICATE, xdr_int, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
name_uid_1(argp, clnt)
	ugname *argp;
	CLIENT *clnt;
{
	static int res;

	bzero(&res, sizeof(res));
	if (clnt_call(clnt, NAME_UID, xdr_ugname, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
group_gid_1(argp, clnt)
	ugname *argp;
	CLIENT *clnt;
{
	static int res;

	bzero(&res, sizeof(res));
	if (clnt_call(clnt, GROUP_GID, xdr_ugname, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


ugname *
uid_name_1(argp, clnt)
	int *argp;
	CLIENT *clnt;
{
	static ugname res;

	bzero(&res, sizeof(res));
	if (clnt_call(clnt, UID_NAME, xdr_int, argp, xdr_ugname, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


ugname *
gid_group_1(argp, clnt)
	int *argp;
	CLIENT *clnt;
{
	static ugname res;

	bzero(&res, sizeof(res));
	if (clnt_call(clnt, GID_GROUP, xdr_int, argp, xdr_ugname, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

