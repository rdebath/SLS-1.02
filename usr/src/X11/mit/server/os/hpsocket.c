/*
 * special socket routine for hp
 */

#include <sys/types.h>
#include <sys/socket.h>

int
set_socket_option (socket_id, option)
int socket_id;
char option;
{
	int optlen = 1;
	char optval = 0x0;

	getsockopt (socket_id, SOL_SOCKET, option, &optval, &optlen);

	optval |= option;

	setsockopt (socket_id, SOL_SOCKET, option, &optval, 1);
}


int
unset_socket_option (socket_id, option)
int socket_id;
char option;
{
	int optlen = 1;
	char optval = 0x0;

	getsockopt (socket_id, SOL_SOCKET, option, &optval, &optlen);

	optval &= ~option;

	setsockopt (socket_id, SOL_SOCKET, option, &optval, 1);
}
