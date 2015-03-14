#include <stdio.h>

#define WHITE	1
#define BLACK	2
#define RED		3
#define ORANGE	4
#define YELLOW	5
#define GREEN	6
#define BLUE	7
#define CYAN	8
#define VIOLET	9
#define LSIDE	10
#define RSIDE	11
#define BOTTOM	12
#define SHADOW	13
#define ULCORN	14
#define URCORN	15
#define BLCORN	16
#define BRCORN	17

int		score_position;		/* position of this game in the hiscore tab */
int 	shape_no;		/* the dripping shape */
int		xpos, ypos, rot;	/* x, y, rotation of shape_no */
int		score;			/* current score */
int		rows;			/* number of rows deleted */
int		next_no;		/* next shape */
int		next_rot;		/* rotation of next shape */
char	*name;			/* username */

#define UWIDTH	10
#define UHEIGHT 22

unsigned char grid[UWIDTH][UHEIGHT];

struct shape_table {
        int     table[4][4];
        int     width;
        int     height;
        int     offset;
        int     pointv[4];
        char    color;
} shape[7];

struct shape {
        int     shape;
        int     rot;
        int     width;
        int     height;
        int     offset;
        int     pointv;
        char    color;
        int     was_shown;
        int     was_shadowed;
} ;
