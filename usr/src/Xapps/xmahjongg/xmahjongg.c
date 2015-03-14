/*
 ******************************************************************************
 *									      *
 *	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
 *	copyright laws of the United States.			      	      *
 *									      *
 ******************************************************************************
 */

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "xmahjongg.h"
#include "variables.h"

main(argc, argv)
int argc;
char *argv[];
{

	get_parameters(argc, argv);

	while(1) {
		initialize();
		keep_playing = 1;
		packet_send(GAME_START);

		while(keep_playing != 0) {
			event_process();
		};
	};
}

get_parameters(argc, argv)
int argc;
char *argv[];
{
	int i, j, c;
	char username[32];
	char *cp, hostname[HOSTNAMELEN];
	Player tmp, *pp = player;

/*
 *	Parse the arguments.
 */
	layout = NULL;
	tile_font = TILE_FONT;

	while ((c = getopt(argc, argv, "icrsb:d:f:l:n:p:")) != EOF) {
		switch (c) {
			case 'b':
				seed = -atoi(optarg);
				break;
			case 'c':
				color_type = 1;
				break;
			case 'd':
				display_name = optarg;
				break;
			case 'f':
				tile_font = optarg;
				break;
			case 'i':
				iconic_start = 1;
				break;
			case 'l':
				layout = optarg;
				break;
#ifndef NO_TCP
			case 'n':
				num_games = atoi(optarg);
				break;
			case 'p':
				tourn_flag = 1;
				cp = strchr(optarg, '@');
				if (cp == NULL) usage(BAD_USERNAME);
				strncpy(pp->name, optarg, cp-optarg);
				strcpy(pp->machine, cp+1);
				num_players++;
				pp++;
				break;
#endif
			case 'r':
				reverse_video = 1;
				break;
			case 's':
				setup_flag = 1;
				break;
			default:
				usage(NULL);
				break;
		};
	};

/*
 *	Check to see if we are in setup mode.  If we are only certain param-
 *	eters are valid.
 */
	if (setup_flag != 0) {
		if (seed < 0) usage(BAD_SEED);
		if (num_games != 0) usage(BAD_GAMES);
		if (tourn_flag != 0) usage(BAD_TOURN);
		if (color_type != 0) usage(BAD_COLOR);

		return(0);
	};

/*
 *	Set up the tournament if requested.
 */
	if (num_players == 0) return(0);
	if (num_games == 0) num_games = 3;

	get_user(username);
	gethostname(hostname, sizeof(hostname));
	mypp = &player[num_players++];
	strcpy(mypp->name, username);
	strcpy(mypp->machine, hostname);

	for (i = 0; i < num_players-1; i++) {
		for (j = i+1; j < num_players; j++) {
			if (strcmp(player[i].name, player[j].name) > 0) {
				tmp = player[i];
				player[i] = player[j];
				player[j] = tmp;
			};
		};
		
	};

	for (i = 0; i < num_players; i++) {
		player[i].x = X_SCORE;
		player[i].y = Y_SCORE+15*i;
		player[i].port = XPORT+i;
		player[i].done = -1;
		player[i].total = 144*num_games;

		for (j = 0; j < MAX_BOARDS; j++) {
			player[i].tiles[j] = 2*TILES;
		};

		if (strcmp(player[i].name, username) < 0) {
			player[i].type = 'C';
		} else if (strcmp(player[i].name, username) > 0) {
			player[i].type = 'A';
		} else {
			mypp = &player[i];
			player[i].type = 'M';
		};
	};

	return(0);
}

usage(s)
char	*s;
{

	if (s != NULL) fprintf(stderr, "xmahjongg: %s\n\n", s);

	fprintf(stderr, "xmahjongg \n");
	fprintf(stderr, "          [-b #]        - board number\n");
	fprintf(stderr, "          [-c]          - color tiles by group\n");
	fprintf(stderr, "          [-d display]  - display name\n");
	fprintf(stderr, "          [-f fontname] - font name\n");
	fprintf(stderr, "          [-i]          - iconic start\n");
	fprintf(stderr, "          [-l file]     - board layout file\n");
#ifndef NO_TCP
	fprintf(stderr, "          [-n #]        - # of games (tournament)\n");
	fprintf(stderr, "          [-p nx@mx]    - another player\n");
#endif
	fprintf(stderr, "          [-r]          - reverse video\n");
	fprintf(stderr, "          [-s]          - setup mode\n");
	exit(1);
}
