/* UNFSD - copyright Mark A Shand, May 1988.
 * This software maybe be used for any purpose provided
 * the above copyright notice is retained.  It is supplied
 * as is, with no warranty expressed or implied.
 */

#include <pwd.h>
#include <grp.h>
#include <fcntl.h>
#include "unfsd.h"

typedef struct mapper {
	struct in_addr	addr;
	struct idm {
		int	*known;
		int	lim;
	} uid, gid, rev_uid, rev_gid;
	struct mapper *next;
} mapper;

static	mapper	*id_maps = NULL;

#ifdef sun
void xdr_free() {}
#endif

static mapper *
getmap(xprt)
SVCXPRT	*xprt;
{
	mapper	**mapp;
	mapper	*map;
	struct in_addr	caller;
	static	mapper mfailed;
	
	caller = svc_getcaller(xprt)->sin_addr;
	for (mapp = &id_maps; *mapp != NULL; mapp = &((*mapp)->next))
		if ((*mapp)->addr.s_addr == caller.s_addr)
		{
			/* move to front */
			if (*mapp != id_maps)
			{
				map = *mapp;
				*mapp = map->next;
				map->next = id_maps;
				id_maps = map;
			}
			return id_maps;
		}
	if ((map = (mapper *) malloc(sizeof *map)) == NULL)
		return &mfailed;
	map->next = id_maps;
	id_maps = map;
	map->addr.s_addr = caller.s_addr;
	map->uid.known = NULL;
	map->uid.lim = 0;
	map->gid.known = NULL;
	map->gid.lim = 0;
	map->rev_uid.known = NULL;
	map->rev_uid.lim = 0;
	map->rev_gid.known = NULL;
	map->rev_gid.lim = 0;
	return map;
}

static int
grow_tab(idmp, sz)
struct	idm	*idmp;
int		sz;
{
	if (idmp->lim == 0)
		idmp->known = (int *) malloc((sz+1) * sizeof(int));
	else
		idmp->known = (int *) realloc((char *) idmp->known, (sz+1) * sizeof(int));
	if (idmp->known == NULL)
		idmp->lim = 0;
	else
	{
		while (idmp->lim <= sz)
			idmp->known[idmp->lim++] = WILDCARD;
		idmp->known[0] = 0;
	}
	return idmp->lim;
}

static int
rlookup(name, id, map, xprt)
char	*name;
int	*id;
int	map;
SVCXPRT	*xprt;
{
	struct sockaddr_in	addr, callback;
	int	cb_fd;
	int	cb_len;
	int	cb_port;
	struct timeval	wait;
	int		sock;
	CLIENT		*cl;
	int		*pi;
	char		**pc;
	int	server_trusted = 0;


	addr = *svc_getcaller(xprt);
	addr.sin_port = 0;
	wait.tv_sec = 10;
	wait.tv_usec = 0;
	sock = RPC_ANYSOCK;

	if ((cl = clntudp_create(&addr, UGIDPROG, UGIDVERS, wait, &sock)) == NULL)
		return 0;
	if ((cb_fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
		goto bad;
	bzero((char *)&callback, sizeof(callback));
	callback.sin_port = 0;
	callback.sin_addr.s_addr = INADDR_ANY;
	callback.sin_family = AF_INET;
	if (bind(cb_fd, &callback, sizeof(callback)) < 0)
		goto bad;
	cb_len = sizeof callback;
	if (getsockname(cb_fd, &callback, &cb_len) < 0)
		goto bad;
	cb_port = ntohs(callback.sin_port);
	pi = authenticate_1(&cb_port, cl);
	if (pi != NULL && *pi == 0)
	{
		struct sockaddr_in	serv;
		int	x;
		int	serv_len;
		struct timeval	timeout;
		fd_set	rset;

		serv_len = sizeof serv;
		timeout.tv_sec = 5;
		timeout.tv_usec = 0;
#ifndef	FD_ZERO
#define FD_ZERO(p)	bzero((char *) (p), sizeof *(p));
#define FD_SETSIZE	32
#define FD_SET(n,p)	((*(int *)(p)) |= 1 << n)
#endif
		FD_ZERO(&rset);
		FD_SET(cb_fd, &rset);
		if (select(FD_SETSIZE, &rset, NULL, NULL, &timeout) > 0)
		{
			recvfrom(cb_fd, &x, sizeof x, 0, &serv, &serv_len);
			server_trusted = ntohs(serv.sin_port) < IPPORT_RESERVED;
		}
	}
	if (!server_trusted)
		goto bad;
	switch (map)
	{
		case NAME_UID:
			pi = name_uid_1(&name, cl);
			if (pi != NULL)
				*id = *pi;
			break;
		case GROUP_GID:
			pi = group_gid_1(&name, cl);
			if (pi != NULL)
				*id = *pi;
			break;
		case UID_NAME:
			pc = uid_name_1(id, cl);
			if ((pi = (int *) pc) != NULL)
				strcpy(name, *pc);
			break;
		case GID_GROUP:
			pc = uid_name_1(id, cl);
			if ((pi = (int *) pc) != NULL)
				strcpy(name, *pc);
			break;
		default:
			pi = NULL;
			break;
	}
	clnt_destroy(cl);
	close(cb_fd);
	return (pi != NULL);
    bad:
	clnt_destroy(cl);
	close(cb_fd);
	return 0;
}

ruid(uid, cp, xprt)
int		uid;
clnt_param	*cp;
SVCXPRT		*xprt;
{
	struct passwd	*pw;
	mapper	*m;

	if (uid < 0)
		return (uid != WILDCARD) ? NOBODY : WILDCARD;
	if (uid == 0 && cp->o.root_squash)
		return NOBODY;
	if (cp->o.uidmap == identity)
		return uid;
	m = getmap(xprt);
	if ((uid < m->uid.lim || grow_tab(&(m->uid), uid))
		&& m->uid.known[uid] == WILDCARD)
	{
		if ((pw = getpwuid(uid)) == NULL)
			m->uid.known[uid] = NOBODY;
		else if (!rlookup(pw->pw_name, &(m->uid.known[uid]), NAME_UID, xprt))
			m->uid.known[uid] = NOBODY;
	}
	return (m->uid.lim == 0) ? NOBODY : m->uid.known[uid];
}

rgid(gid, cp, xprt)
int		gid;
clnt_param	*cp;
SVCXPRT		*xprt;
{
	struct group	*gr;
	mapper	*m;

	if (gid < 0)
		return (gid != WILDCARD) ? NOBODY : WILDCARD;
	if (cp->o.uidmap == identity)
		return gid;
	m = getmap(xprt);
	if ((gid < m->gid.lim || grow_tab(&(m->gid), gid))
		&& m->gid.known[gid] == WILDCARD)
	{
		if ((gr = getgrgid(gid)) == NULL)
			m->gid.known[gid] = NOBODY;
		else if (!rlookup(gr->gr_name, &(m->gid.known[gid]), GROUP_GID, xprt))
			m->gid.known[gid] = NOBODY;
	}
	return (m->gid.lim == 0) ? NOBODY : m->gid.known[gid];
}

luid(uid, cp, xprt)
int		uid;
clnt_param	*cp;
SVCXPRT		*xprt;
{
	struct passwd	*pw;
	mapper	*m;
	char	namebuf[MAXUGLEN];

	if (uid < 0)
		return (uid != WILDCARD) ? NOBODY : WILDCARD;
	if (uid == 0 && cp->o.root_squash)
		return NOBODY;
	if (cp->o.uidmap == identity)
		return uid;
	m = getmap(xprt);
	if ((uid < m->rev_uid.lim || grow_tab(&(m->rev_uid), uid))
		&& m->rev_uid.known[uid] == WILDCARD)
	{
		if (!rlookup(namebuf, &uid, UID_NAME, xprt))
			m->rev_uid.known[uid] = NOBODY;
		else if ((pw = getpwnam(namebuf)) == NULL)
			m->rev_uid.known[uid] = NOBODY;
		else
			m->rev_uid.known[uid] = pw->pw_uid;
	}
	return (m->rev_uid.lim == 0) ? NOBODY : m->rev_uid.known[uid];
}

lgid(gid, cp, xprt)
int		gid;
clnt_param	*cp;
SVCXPRT		*xprt;
{
	struct group	*gr;
	mapper	*m;
	char	namebuf[MAXUGLEN]; 

	if (gid < 0)
		return (gid != WILDCARD) ? NOBODY : WILDCARD;
	if (cp->o.uidmap == identity)
		return gid;
	m = getmap(xprt);
	if ((gid < m->rev_gid.lim || grow_tab(&(m->rev_gid), gid))
		&& m->rev_gid.known[gid] == WILDCARD)
	{
		if (!rlookup(namebuf, &gid, GID_GROUP, xprt)) 
			m->rev_gid.known[gid] = NOBODY;
		else if ((gr = getgrnam(namebuf)) == NULL)
			m->rev_gid.known[gid] = NOBODY;
		else
			m->rev_gid.known[gid] = gr->gr_gid;
	}
	return (m->rev_gid.lim == 0) ? NOBODY : m->rev_gid.known[gid];
}
