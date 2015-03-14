/*
 ******************************************************************************
 *									      *
 *	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
 *	copyright laws of the United States.			      	      *
 *									      *
 ******************************************************************************
 */

/*
 *	Include file for xmahjongg
 */

/*
 *	Path used for layout files
 */
#define	LAYOUT		"/usr/local/lib/xmahjongg"

/*
 *	Fundamental definitions
 */
typedef unsigned char	Uchar;
typedef unsigned long	Ulong;

#define	TILES		144

#define ROWS		15
#define COLS		29
#define LEVS		7

#define	TRUE		1
#define	FALSE		0

#define FREE		0
#define	USED		1

/*
 *	Dimensions for boxes, tiles and options
 */
#define	TILE_SIDE	64

#define	ICON_WIDTH	64
#define	ICON_HEIGHT	64

#define	PLAY_WIDTH	1000
#define	PLAY_HEIGHT	616

#define	COUNT_WIDTH	192
#define	COUNT_HEIGHT	64

#define	SHADE_WIDTH	4
#define	SHADE_HEIGHT	64

#define	OPTION_WIDTH	64
#define	OPTION_HEIGHT	32

#define WINDOW_WIDTH	1000
#define WINDOW_HEIGHT	752

#define BORDER_WIDTH	2

/*
 *	Upper left coordinates for items
 */
#define X_TILE		10
#define Y_TILE		40

#define X_PLAY		-2
#define Y_PLAY		178

#define X_DONE		800
#define Y_DONE		100

#define X_SAME		800
#define Y_SAME		30

#define X_QUIT		900
#define Y_QUIT		100

#define X_NEW		900
#define Y_NEW		30

#define	X_COPY		800
#define	Y_COPY		150

#define	X_RIGHTS	800
#define	Y_RIGHTS	162

#define	X_COUNT		55
#define	Y_COUNT		100

#define	X_BOARD		345
#define	Y_BOARD		40

#define	X_NAMES		260
#define	Y_NAMES		55

#define	X_SCORE		345
#define	Y_SCORE		55

#define in_bounds(x,y)	((0 <= x) && (x < ROWS) && (0 <= y) && (y < COLS))

/*
 *	Tile structure
 */
typedef	struct tile	Tile;

struct tile {
	int	x;
	int	y;
	int	row;
	int	col;
	int	lev;
	int	data;
	int	type;
	int	init;
	int	state;
	long	value;
	Tile	*flink;
	Tile	*blink;
};

#define	INDEX_NUMBER		0x01
#define	INDEX_BAMBOO		0x11
#define	INDEX_CIRCLE		0x21
#define	INDEX_IDEOGRAPH		0x31
#define	INDEX_DIRECTION		0x41
#define	INDEX_PLANT		0x51
#define	INDEX_SEASON		0x61
#define	INDEX_DRAGON		0x71

#define	INDEX_DONE		0x5a
#define	INDEX_NEW		0x5b
#define	INDEX_QUIT		0x5c
#define	INDEX_SAME		0x5d

#define	INDEX_A			0x66
#define	INDEX_G			0x67
#define	INDEX_H			0x68
#define	INDEX_J			0x69
#define	INDEX_M			0x6a
#define	INDEX_N			0x6b
#define	INDEX_O			0x6c
#define	INDEX_X			0x6d

/*
 *	Tournament mode variables and structures
 */
#define	XPORT		3657		/* port value for tournaments */
#define	MAX_PLAYERS	5
#define MAX_BOARDS	7
#define	HOSTNAMELEN	256

typedef	struct player	Player;

struct player {
	char	name[12];
	char	machine[HOSTNAMELEN];
	long	x;
	long	y;
	long	fd;
	long	type;
	long	port;
	long	done;
	long	quit;
	long	total;
	long	tiles[MAX_BOARDS];
	long	board[MAX_BOARDS];
};

typedef struct packet	Packet;

struct packet {
	short	type;
	short	port;
	short	extra;
	short	tiles;
	long	board;
	char	name[12];
};

#define	GAME_START	1
#define	GAME_PLAY	2
#define	GAME_DONE	3
#define	GAME_QUIT	4

/*
 *	X Window stuff
 */
#define TEXT_FONT	"6x10"
#define TILE_FONT	"xmahjongg"
#define XGameEvents	ButtonPressMask | ExposureMask | StructureNotifyMask

/*
 *	Error messages
 */
#define	BAD_USERNAME	"invalid user@machine syntax"
#define	BAD_SEED	"-b not allowed with -s"
#define	BAD_GAMES	"-n not allowed with -s"
#define	BAD_TOURN	"-p not allowed with -s"
#define	BAD_COLOR	"-c not allowed with -s"
