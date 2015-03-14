/*
 *  If TRUE and FALSE isn't defined, we define our own
 *
 */

#ifndef TRUE
#define TRUE (1)
#define FALSE (0)
#endif

/*
 * Definitions for the keys to control the menus.
 *
 */

#define MV_UP 'k'
#define MV_DOWN 'j'
#define PG_UP 'K'
#define PG_DOWN 'J'
#define QUIT 27
#define SELECT 13
#define HELP '?'

/*
 * Definitions for menu flags
 *
 */

#define WRAP 0x01		/* If the menu should wrap */
#define DRAW_BOX  0x02          /* If the menu should have a box */
#define ENABLE_QUIT 0x04        /* If the menu should accept QUIT key */
 				/* and return -1                      */
#define ENABLE_HELP 0x08        /* If the menu should accept help key */
#define CLEAR_ON_EXIT 0x10	/* Clear the menuwindow on exit */

#define DRAW_BOX_SYMBOL_HORIZ '-'
#define DRAW_BOX_SYMBOL_VERT '?'

#define SELECTABLE 0x01

/*
 * Types for the menu
 *
 */

typedef struct {
  char *name;
  char *help;
  int flags;
} MenuEntry;

int menu( int x, int y, int width, int height, int start, int flags, MenuEntry *names );
