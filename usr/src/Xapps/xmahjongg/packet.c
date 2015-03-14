/*
 ******************************************************************************
 *									      *
 *	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
 *	copyright laws of the United States.			      	      *
 *									      *
 ******************************************************************************
 */

#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <netinet/in.h>
#include "xmahjongg.h"
#include "variables.h"

packet_send(type)
int type;
{
	int i;
	int packetlen = sizeof(Packet);
	char ascii[24];

	if (tourn_flag == 0) return(0);
	i = mypp->done;

/*
 *	Update the information on my board
 */
	if (type == GAME_START) {
		mypp->done++;
		mypp->tiles[mypp->done] = TILES;
		mypp->board[mypp->done] = seed;
		if ((i >= 0) && (mypp->tiles[i] > 0)) mypp->tiles[i] *= -1;
		draw_user(mypp, GAME_START);
	} else if (type == GAME_PLAY) {
		mypp->total -= 2;
		mypp->tiles[i] = tiles_remaining;
		draw_user(mypp, GAME_PLAY);
	} else if (type == GAME_DONE) {
		if (mypp->tiles[i] > 0) mypp->tiles[i] *= -1;
		draw_user(mypp, GAME_DONE);
	} else if (type == GAME_QUIT) {
		draw_user(mypp, GAME_QUIT);
	} else {
		fprintf(stderr, "bad packet type = %d\n", type);
		exit(1);
	};

/*
 *	Send the information to the other players
 */
	packet.type = htons(type);
	packet.port = htons(mypp->port);
	packet.tiles = htons(tiles_remaining);
	packet.board = htonl(seed);
	strcpy(packet.name, mypp->name);

	for (i = 0, pp = player; i < num_players; i++, pp++) {
		if ((pp->fd < 0) || (pp->type == 'M')) continue;
		if (write(pp->fd, (char *)&packet, packetlen) != packetlen) {
			fprintf(stderr, "can't send to %s\n", pp->name);
			playfds ^= (1 << pp->fd);
			close(pp->fd);
			pp->fd = -1;
		};
	};

	return(0);
}

packet_recv(fd)
int fd;
{
	int i;
	int readfds = 1 << fd;
	int packetlen = sizeof(Packet);
	struct timeval timeout;
	char ascii[32];
	char *cp;

/*
 *	Read in the packet which is waiting for us.
 */
	timeout.tv_sec = 0L;
	timeout.tv_usec = 0L;

	if (select(maxfds, &readfds, NULL, NULL, &timeout) <= 0) {
		return(0);
	} else if ((i = read(fd, (char *)&packet, packetlen)) != packetlen) {
		fprintf(stderr, "can't read from %s\n", pp->name);
		playfds ^= (1 << pp->fd);
		close(pp->fd);
		pp->fd = -1;
		return(0);
	};

/*
 *	Find this players structure.
 */
	packet.type = ntohs(packet.type);
	packet.port = ntohs(packet.port);
	packet.tiles = ntohs(packet.tiles);
	packet.board = ntohl(packet.board);

	for (i = 0, pp = player; i < num_players; i++, pp++) {
		if (pp->port == packet.port) break;
	};

	if (i == num_players) {
		fprintf(stderr, "****** BAD PACKET ******\n");
		fprintf(stderr, "packet name = %s   port=%d\n",
			packet.name, packet.port);
		return(0);
	};

	i = pp->done;

	switch (packet.type) {
		case GAME_START:
			pp->done++;
			pp->board[i+1] = packet.board;
			pp->tiles[i+1] = packet.tiles;
			if ((i >= 0) && (pp->tiles[i] > 0)) pp->tiles[i] *= -1;
			draw_user(pp, GAME_START);
			break;
		case GAME_PLAY:
			pp->total -= 2;
			pp->tiles[i] = packet.tiles;
			draw_user(pp, GAME_PLAY);
			break;
		case GAME_DONE:
			if (pp->tiles[i] > 0) pp->tiles[i] *= -1;
			draw_user(pp, GAME_DONE);
			break;
		case GAME_QUIT:
			pp->tiles[i] = -packet.tiles;
			pp->quit = 1;
			draw_user(pp, GAME_QUIT);
			playfds ^= (1 << pp->fd);
			close(pp->fd);
			pp->fd = -1;
			break;
		default:
			fprintf(stderr, "bad switch (%d)\n", packet.type);
			exit(1);
			break;
	};

	return(0);
}
