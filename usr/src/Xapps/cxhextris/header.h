/*
 * hextris Copyright 1990 David Markley, dm3e@+andrew.cmu.edu, dam@cs.cmu.edu
 *
 * Permission to use, copy, modify, and distribute, this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders be used in
 * advertising or publicity pertaining to distribution of the software with
 * specific, written prior permission, and that no fee is charged for further
 * distribution of this software, or any modifications thereof.  The copyright
 * holder make no representations about the suitability of this software for
 * any purpose.  It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDER DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA, PROFITS, QPA OR GPA, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE 
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

/* The maximum rows in the game.
 */
#define MAXROW 26
/* The maximum columns in the game.
 */
#define MAXCOLUMN 13
/* The number of high scores that are kept.
 */
#define MAXHIGHSCORES 500
/* The maximum length of the user defined name.
 * The user can set his hextris playing name by setting the
 * environment variable XHEXNAME, like this:
 * setenv XHEXNAME "Mr. Xhextris"
 */
#define MAXNAMELENGTH 40
/* The maximum length of the users id.
 */
#define MAXUSERIDLENGTH 40
/* The maximum nuber of high scores a user can have in the high score file.
 */
#define MAXUSERHIGHS 3
/* The number of pieces in the game. DON'T CHANGE!!!
 */
#define NUMBEROFPIECES 10
/* The name of the xhextris font. DON'T CHANGE!!!
 */
#define HEXFONTNAME "hex20"
/* The directory where the text font is.
 */
#define FONTDIR "/usr/lib/X11/fonts/misc/"
/* The text font being used.
 */
#define FONTNAME "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" /*8x13B"*/
/* The name on the window.
 */
#define WINDOWNAME "xhextris"
/* The name on the icon.
 */
#define ICONNAME "xhextris"

/* This is the type definition for a piece.
 */
typedef struct piece_s
{
    int	type;
    int	rotation;
    int row;
    int column;
} piece_t;

/* This is the type definition for a position.
 */
typedef struct position_s
{
    int type;
    int filled;
} position_t;

/* This is the type definition for a high score.
 */
typedef struct high_score_s
{
    char name[MAXNAMELENGTH];
    char userid[MAXUSERIDLENGTH];
    int score;
    int rows;
} high_score_t;

/* This is the mess that defines each piece. There are six rotations to
 * each piece, so there are six rows of numbers to each piece. For example,
 * the second rotation of the third piece is on the row labled 21. Each row
 * has 16 numbers, that can be broken down into 2 sets of 4 pairs. Each pair
 * is a set of relative coordinates inf row and column from the current
 * position of the piece. There are two sets of 4 pairs, because the offsets
 * differ depending on whether the current position of the piece is in a
 * hex that is up to its neighbors in its row, or down to them. One row in the
 * game moves up and down, from left to right.
 */
static int shape[NUMBEROFPIECES*6][16]
  = {{0,0,-1,0,1,-1,1,1,-1,0,0,-1,0,1,0,0},       /* 00 */
       {0,0,0,-1,0,1,1,0,-1,-1,-1,1,1,0,0,0},     /* 01 */
       {0,0,-1,0,1,-1,1,1,-1,0,0,-1,0,1,0,0},     /* 02 */
       {0,0,0,-1,0,1,1,0,-1,-1,-1,1,1,0,0,0},     /* 03 */
       {0,0,-1,0,1,-1,1,1,-1,0,0,-1,0,1,0,0},     /* 04 */
       {0,0,0,-1,0,1,1,0,-1,-1,-1,1,1,0,0,0},     /* 05 */
       {0,0,-1,0,1,0,2,0,-1,0,1,0,2,0,0,0},       /* 10 */
       {0,0,0,-1,1,1,1,2,-1,-1,0,1,1,2,0,0},      /* 11 */
       {0,0,1,-1,0,1,-1,2,0,-1,-1,1,-1,2,0,0},    /* 12 */
       {0,0,-2,0,-1,0,1,0,-2,0,-1,0,1,0,0,0},     /* 13 */
       {0,0,-1,-2,0,-1,1,1,-1,-2,-1,-1,0,1,0,0},  /* 14 */
       {0,0,1,-2,1,-1,0,1,1,-2,0,-1,-1,1,0,0},    /* 15 */
       {0,0,-1,0,1,0,1,1,-1,0,1,0,0,1,0,0},       /* 20 */
       {0,0,0,-1,1,1,0,1,-1,-1,0,1,-1,1,0,0},     /* 21 */
       {0,0,1,-1,0,1,-1,0,0,-1,-1,0,-1,1,0,0},    /* 22 */
       {0,0,0,-1,-1,0,1,0,-1,-1,-1,0,1,0,0,0},    /* 23 */
       {0,0,0,-1,1,-1,1,1,0,-1,-1,-1,0,1,0,0},    /* 24 */
       {0,0,1,-1,1,0,0,1,0,-1,1,0,-1,1,0,0},      /* 25 */
       {0,0,-1,0,1,-1,1,0,-1,0,0,-1,1,0,0,0},     /* 30 */
       {0,0,0,-1,1,0,1,1,-1,-1,1,0,0,1,0,0},      /* 31 */
       {0,0,1,-1,0,1,1,1,0,-1,-1,1,0,1,0,0},      /* 32 */
       {0,0,-1,0,1,0,0,1,-1,0,1,0,-1,1,0,0},      /* 33 */
       {0,0,0,-1,-1,0,1,1,-1,-1,-1,0,0,1,0,0},    /* 34 */
       {0,0,0,-1,1,-1,0,1,-1,-1,0,-1,-1,1,0,0},   /* 35 */
       {-1,0,0,-1,1,-1,1,0,-1,0,-1,-1,0,-1,1,0},  /* 40 */
       {0,-1,1,-1,1,0,1,1,-1,-1,0,-1,1,0,0,1},    /* 41 */
       {1,-1,1,0,1,1,0,1,0,-1,1,0,0,1,-1,1},      /* 42 */
       {1,0,1,1,0,1,-1,0,1,0,0,1,-1,1,-1,0},      /* 43 */
       {1,1,0,1,-1,0,0,-1,0,1,-1,1,-1,0,-1,-1},   /* 44 */
       {0,1,-1,0,0,-1,1,-1,-1,1,-1,0,-1,-1,0,-1}, /* 45 */
       {-1,0,1,1,2,1,0,0,-1,0,0,0,0,1,1,1},       /* 50 */
       {0,-1,0,0,0,1,0,2,-1,-1,0,0,-1,1,0,2},     /* 51 */
       {1,-1,0,0,-1,0,-1,1,0,-1,0,0,-1,0,-2,1},   /* 52 */
       {1,0,0,0,0,-1,-1,-1,1,0,0,0,-1,-1,-2,-1},  /* 53 */
       {0,-2,1,-1,0,0,1,1,0,-2,0,-1,0,0,0,1},     /* 54 */
       {2,-1,1,0,0,0,0,1,1,-1,1,0,0,0,-1,1},      /* 55 */
       {0,0,1,-1,1,0,1,1,0,0,0,-1,1,0,0,1},	  /* 60 */
       {0,0,1,0,1,1,0,1,0,0,1,0,0,1,-1,1},	  /* 61 */
       {0,0,1,1,0,1,-1,0,0,0,0,1,-1,1,-1,0},      /* 62 */
       {0,0,0,1,-1,0,0,-1,0,0,-1,1,-1,0,-1,-1},   /* 63 */
       {0,0,-1,0,0,-1,1,-1,0,0,-1,0,-1,-1,0,-1},  /* 64 */
       {0,0,0,-1,1,-1,1,0,0,0,-1,-1,0,-1,1,0},    /* 65 */
       {-1,0,0,0,1,0,2,1,-1,0,0,0,1,0,1,1},	  /* 70 */
       {0,-1,0,0,1,1,0,2,-1,-1,0,0,0,1,0,2},      /* 71 */
       {1,-1,0,0,0,1,-1,1,0,-1,0,0,-1,1,-2,1},    /* 72 */
       {1,0,0,0,-1,0,-1,-1,1,0,0,0,-1,0,-2,-1},   /* 73 */
       {0,-2,0,-1,0,0,1,1,0,-2,-1,-1,0,0,0,1},    /* 74 */
       {2,-1,1,-1,0,0,0,1,1,-1,0,-1,0,0,-1,1},    /* 75 */
       {-1,0,0,0,1,0,2,-1,-1,0,0,0,1,0,1,-1},     /* 80 */
       {0,-1,0,0,1,1,2,1,-1,-1,0,0,0,1,1,1},      /* 81 */
       {1,-1,0,0,0,1,0,2,0,-1,0,0,-1,1,0,2},      /* 82 */
       {1,0,0,0,-1,0,-1,1,1,0,0,0,-1,0,-2,1},     /* 83 */
       {-1,-1,0,-1,0,0,1,1,-2,-1,-1,-1,0,0,0,1},  /* 84 */
       {0,-2,1,-1,0,0,0,1,0,-2,0,-1,0,0,-1,1},    /* 85 */
       {-1,0,0,0,1,-1,2,-1,-1,0,0,0,0,-1,1,-1},   /* 90 */
       {0,-1,0,0,1,0,2,1,-1,-1,0,0,1,0,1,1},      /* 91 */
       {1,-1,0,0,1,1,0,2,0,-1,0,0,0,1,0,2},       /* 92 */
       {1,0,0,0,0,1,-1,1,1,0,0,0,-1,1,-2,1},      /* 93 */
       {1,1,0,0,-1,0,-1,-1,0,1,0,0,-1,0,-2,-1},   /* 94 */
       {0,-2,0,-1,0,0,0,1,0,-2,-1,-1,0,0,-1,1}};  /* 95 */

#ifdef LOG
#define LOGHOST "waddington.andrew.cmu.edu"
char log_message[80];
#endif

extern int score_cardinal;
