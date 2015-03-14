 unsigned long *A;
 unsigned long *B;
/* void nputc(SCRN *t,int x,int y,int c);
 *
 * Write a character to the screen.  Warning:  This macro accesses parameter
 * 't' more than once.
 */
#define nputc(t,x,y,c) \
 ( \
 ( (t)->quickies==NQUICK ? ndumpq(t) : 0 ), \
 (t)->quick[(t)->quickies].ch=1, \
 (t)->quick[(t)->quickies].x=(x), \
 (t)->quick[(t)->quickies].y=(y), \
 (t)->quick[(t)->quickies].attr=(t)->tattr, \
 (t)->quick[(t)->quickies].c=(c), \
 ++(t)->quickies \
 )

/* void nputs(SCRN *t,int x,int y,char *s);
 *
 * Write a string to the screen.  The string must never go past the right edge
 * of the screen.
 */
void nputs();

/* void nprintf(SCRN *t,int x,int y,char *str,...);
 *
 * Printf to the screen using the current attributes.  The string should never
 * go past the right edge of the screen.
 */
void nprintf();

/* void nsetattr(SCRN *t,int c);
 *
 * Set the attribute value.  This value is ORed with the characters written
 * by 'nputs', 'nputc', 'nprintf', 'nbox' and 'nrect' before being sent to the
 * screen.
 */
#define nsetattr(t,c) ((t)->tattr=(c))
/* int ngetattr(SCRN *t);
 *
 * Get the current attributes
 */
#define ngetattr(t) ((t)->tattr)

/* int navattr(SCRN *t);
 *
 * Get attributes available with this terminal.  I.E., the returned word
 * is the some of the attribute bit values for the attributes which are
 * available.
 */
#define navattr(t) ((t)->avattr)

/* void nsetpos(SCRN *t,int x,int y);
 *
 * Set the cursor position on the screen.  This is where the cursor will
 * be left after calling 'nrefresh' or 'ngetc'.
 */
void nsetpos();

/* void ngetpos(SCRN *t,int *x,int *y);
 *
 * Sets the variables at the addresses given with the real cursor position.
 */
void ngetpos();

/* void nrect(SCRN *t,int x,int y,int w, int h,int c);
 *
 * Fill a rectangle defined by 'x', 'y', 'w' and 'h' with the character 'c'.
 * The current attributes (set with 'nattr') are ORed in with the character.
 */
void nrect();

/* void nbox(SCRN *t,int x,int y,int w,int h);
 *
 * Draw a box on the screen.  'x', 'y', 'w' and 'h' give specify the rectangle
 * the box will surround.  The global variables 'boxul', 'boxur', 'boxll',
 * 'boxlr', 'boxl', 'boxr', 'boxt', 'boxb' specify the characters to use to
 * draw the box.  The current attributes (set with 'nattr') are ORed with
 * these characters before the box is drawn.  These characters are initialized
 * with '+', '-', and '|' and may be changed.
 */
void nbox();
extern int boxl, boxr, boxt, boxb, boxll, boxlr, boxul, boxur;


/* int *nsave(SCRN *t,int x,int y,int w,int h);
 *
 * Save a rectangle in an malloc block.  The address of the malloc block is
 * returned.  The format of the array is always: array[x+y*width]
 */
int *nsave();

/* void nrestore(SCRN *t,int x,int y,int w,int h,int *s);
 *
 * Write an array (as returned by 'nsave') to the screen.  This routine does
 * not free the malloc block returned by 'nsave'.
 */
void nrestore();

/* void nrefresh(SCRN *t);
 *
 * Refresh the screen.  This should never have to be called except for
 * animation or 'PLEASE WAIT, READING FROM DISK' messages.  'ngetc'
 * automatically calls this.  If there is pending input, and if this can be
 * detected by the TTY driver, 'nrefresh' aborts.
 */
void nrefresh();

/* void ndumpq(SCRN *t);
 *
 * Dump buffered screen modifications to the screen buffer.  This routine
 * should be called before any direct access to 't->screen' array.  It will
 * make sure that any updates done with 'nputc', 'nputs' and 'nprintf' are
 * made to the screen array.  This way, you can write the array directly
 * messing up the order of the screen modifications
 */
void ndumpq();


/* int NDIRECT(SCRN *t,int x,int y)
 *
 * Macro to directly access the screen buffer.
 */
#define NDIRECT(t,x,y) ((t)->screen[(x)+(y)*(t)->co])


/* void ngosling(SCRN *t);
 *
 * Apply Jame's Gosling's dynamic programming algorithm for optimized scrolling
 * once.  This function determines the best set of scrolls which will help make
 * the current terminal screen look like what you want.  (The algorithm has
 * been modified to use scrolling regions instead of insert/delete line).
 *
 * This function is expensive because it has to read through the screen
 * buffers to generate single integer IDs for each line.  Because of this,
 * 'ngosling' should only be called when you know that optimal scrolling
 * will help with the screen update.
 */
void ngosling();

