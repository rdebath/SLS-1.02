#include "defs.h"

#include <stdlib.h>
#include <sys/types.h>
#include <setjmp.h>
#include <pwd.h>
#include <ncurses.h>

#define VUNIT       1			/* ver unit */
#define HUNIT		2			/* hor unit */
#define NORM_OFFSET	15			/* x offset for blocks */
#define NEXT_HEIGHT	(VUNIT*5)	/* Height for next block */

static char *blostr = "  ";		/* char used to draw blocks */
static jmp_buf rest;			/* to restart the game */
static int bp = 1;
static int beginlev = 0;

main(argc, argv)
int     argc;
char  **argv;
{
	int i, c;

	for ( i=1; i < argc; i++ ) {
		if ( argv[i][0] == '-' ) switch ( argv[i][1] ) {
		case 's' :
			bp = 0;
			break;
		case 'l' :
			if ( argv[i][2] )
				beginlev = atoi(argv[i]+2);
			else
				beginlev = atoi(argv[++i]);
			if ( beginlev < 0 )
				beginlev = 0;
			break;
		case 'b' :
			if ( argv[i][2] )
				c = argv[i][2];
			else
				c = argv[++i][0];
			blostr[0] = blostr[1] = c;
			break;
		}
	}
    initialise();
	setjmp(rest);
	init_all();
    main_loop();
    exit(0);
}

initialise()
{
    struct passwd *who;
	int end_proc();

	init_tty();
	print_authors();
    srand((unsigned) time((time_t *) 0));
    define_shapes();
	who = (struct passwd *)getpwuid(getuid());
	name = who->pw_name;
    read_high_scores();
}

init_all()
{
   int     i, j;

	score_position = -1;
    rows = score = shape_no = rot = xpos = ypos = 0;
    for (i = 0; i < UWIDTH; i++)
        for (j = 0; j < UHEIGHT; j++)
            grid[i][j] = 0;
    create_shape();         /* Set up 1st shape */
    create_shape();         /* Set up next shape */
  	clear();
    setpixels(NORM_OFFSET-1, 0, 1, UHEIGHT , LSIDE);
	set1pixel(NORM_OFFSET-1, UHEIGHT, BLCORN);
    setpixels(NORM_OFFSET+UWIDTH, 0, 1, UHEIGHT , RSIDE);
	set1pixel(NORM_OFFSET+UWIDTH, UHEIGHT, BRCORN);
    show_score(1);
    show_next(1);
    draw_shadow(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
}

init_tty()
{

	initscr();
	leaveok(stdscr,FALSE);
	cbreak();
	noecho();
	nodelay(stdscr, TRUE);
	keypad(stdscr, TRUE);
}

static int timings[] = { 60000, 50000, 40000, 30000, 20000, 16000};
static int  currtim;

setlevel(level)
{
	if ( level >= sizeof(timings) )
		level = sizeof(timings) - 1;
	currtim = timings[level];
}

main_loop()
{
	register int nc;

	for ( setlevel(beginlev); ; ) {
		nc = getch();
		switch ( nc ) {
		case 'k' :
		case KEY_UP:
			anti_proc();
			break;
		case 'h' :
		case KEY_LEFT: 
			left_proc();
			break;
		case 'j' :
		case KEY_DOWN:
			clock_proc();
			break;
		case 'l' :
		case KEY_RIGHT:
			right_proc();
			break;
		case ' ' :
			fast_proc();
			break;
		case 'q' :
		case 'Q' :
			end_proc();
			break;
		default:
			drop_block();
			break;
		}
		usleep(currtim);
	}
}

drop_block()
{
	register int orows;

        if (block_can_drop(shape_no, xpos, ypos, rot))
                print_shape(shape_no, xpos + NORM_OFFSET, ypos++, rot, WHITE);
        else {
                if (ypos < 0)
                        end_game();
                else {
					orows = rows;
                    score += shape[shape_no].pointv[rot];
                    store_shape(shape_no, xpos, ypos, rot);
                    remove_full_lines(ypos);
                    create_shape();
                    show_score(1);
                    show_next(1);
					if ( orows != rows ) {
						scbeep();
						setlevel(beginlev + (rows/10));
					}
                }
        }
        print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
        draw_shadow(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
}

static show_score(on)
{
	register int y, x;
        char    b1[32], b2[32], b3[32];
	char	*p1,	*p2,	*p3;

	if ( on ) {
		sprintf(p1 = b1, "S C O R E : %d", score);
		sprintf(p2 = b2, "L e v e l : %d", beginlev + (rows/10) );
		sprintf(p3 = b3, "R o w s   : %d", rows);
	} else
		p1 = p2 = p3 = "                    ";
	x = NORM_OFFSET + UWIDTH + 5;
	y = 1;
	wtext(x, y , p1, 0);
	wtext(x, y + 2, p2, 0);
	wtext(x, y + 4, p3, 0);
}

int waituser(echon) 
{
	int c;

	refresh();
	nodelay(stdscr, FALSE);
	c = getch();
	nodelay(stdscr, TRUE);
	return c;
}

end_game() 
{
	int c;

	setpixels(9, 6, 22 , 10, WHITE);
	set1pixel(9,6,ULCORN);
	setpixels(10, 6, 20 , 1, BOTTOM);
	set1pixel(30,6,URCORN);
	setpixels(9, 7, 1 , 9, LSIDE);
	setpixels(30, 7, 1 , 9, RSIDE);
	set1pixel(9,16,BLCORN);
	setpixels(10, 16, 20 , 1, BOTTOM);
	set1pixel(30,16,BRCORN);
	wtext(15,10,"  G A M E  O V E R  ", 1);
	wtext(14,13," press space to continue ",1);
	scbeep();
	waituser();
    update_highscore_table();
	print_high_scores();
	wtext(15,20,"  Another game ?  ", 0);
	c = waituser(1);
	if ( c == 'n' || c == 'N' || c == 'q' || c == 'Q' )
		end_proc();
	longjmp(rest,1);
}

end_proc()
{
	clear();
	wtext(15,LINES-6,"                    ",1);
	wtext(15,LINES-5,"    Play Tetris!    ",1);
	wtext(15,LINES-4,"                    ",1);
	move(LINES-1,0);
	refresh();
	endwin();
	exit(0);
}

left_proc()
{
        if (block_can_left(shape_no, xpos, ypos, rot)) {
                print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, WHITE);
                xpos--;
                print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
                draw_shadow(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
        }
}

right_proc()
{
        if (block_can_right(shape_no, xpos, ypos, rot)) {
                print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, WHITE);
                xpos++;
                print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
                draw_shadow(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
        }
}

anti_proc()
{
        int     newrot;

        newrot = (rot + 3) % 4;
        if (check_rot(shape_no, xpos, ypos, newrot)) {
                print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, WHITE);
                rot = newrot;
                print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
                draw_shadow(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
        }
}

clock_proc()
{
        int     newrot;

        newrot = (rot + 1) % 4;
        if (check_rot(shape_no, xpos, ypos, newrot)) {
                print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, WHITE);
                rot = newrot;
                print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
                draw_shadow(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
        }
}

fast_proc()
{
        while (block_can_drop(shape_no, xpos, ypos, rot)) {
                print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, WHITE);
                ypos++;
                print_shape(shape_no, xpos + NORM_OFFSET, ypos, rot, shape[shape_no].color);
        }
}

/*	Drawing primitives
 */
draw_shadow(shape_no, xpos, ypos, rot, col)
        int     shape_no, xpos, ypos, rot, col;
{
        int     y1;
        int     x1, x2, x3, x4;
        int     t0, t1, t2, t3;

        t0 = shape[shape_no].table[0][rot];     /* Bit map of 1st Row */
        t1 = shape[shape_no].table[1][rot];     /* Bit map of 2nd Row */
        t2 = shape[shape_no].table[2][rot];     /* Bit map of 3rd Row */
        t3 = shape[shape_no].table[3][rot];     /* Bit map of 4th Row */

        x1 = xpos;       /* Position of 1st column of block grid */
        x2 = x1+1;         /* Position of 2nd column of block grid */
        x3 = x2+1;         /* Position of 3rd column of block grid */
        x4 = x3+1;         /* Position of 4th column of block grid */

        y1 = UHEIGHT;

        setpixels(NORM_OFFSET, y1, UWIDTH, 1, BOTTOM);
        if (t0 & 8 || t1 & 8 || t2 & 8 || t3 & 8)
                set1pixel(x1, y1, SHADOW);
        if (t0 & 4 || t1 & 4 || t2 & 4 || t3 & 4)
                set1pixel(x2, y1, SHADOW);
        if (t0 & 2 || t1 & 2 || t2 & 2 || t3 & 2)
                set1pixel(x3, y1, SHADOW);
        if (t0 & 1 || t1 & 1 || t2 & 1 || t3 & 1)
                set1pixel(x4, y1, SHADOW);
}

show_next(on)
{
        int     y;

        y = 1;
        setpixels(0, y , UWIDTH, NEXT_HEIGHT - 1, WHITE);
	if ( on ) {
		wtext(0, y , " N e x t : ", 0);
		print_shape(next_no, 6, y, next_rot, shape[next_no].color);
	}
}

print_shape(shape_no, x, y, rot, col)
        int     shape_no, x, y, rot, col;
{
        int     x1, x2, x3, x4, y1;
        int     t0, t1, t2, t3;

        t0 = shape[shape_no].table[0][rot];     /* Bit map of 1st Row */
        t1 = shape[shape_no].table[1][rot];     /* Bit map of 2nd Row */
        t2 = shape[shape_no].table[2][rot];     /* Bit map of 3rd Row */
        t3 = shape[shape_no].table[3][rot];     /* Bit map of 4th Row */

        x1 = x;          /* Position of 1st column of block grid */
        x2 = x1 + 1;         /* Position of 2nd column of block grid */
        x3 = x2 + 1;         /* Position of 3rd column of block grid */
        x4 = x3 + 1;         /* Position of 4th column of block grid */
        y1 = y;          /* Position of 1st row of block grid    */

        if (y > -1) {
                if (t0 & 8)
                        set1pixel(x1, y1, col);
                if (t0 & 4)
                        set1pixel(x2, y1, col);
                if (t0 & 2)
                        set1pixel(x3, y1, col);
                if (t0 & 1)
                        set1pixel(x4, y1, col);
        }
        y1 += 1;             /* Position of next row */
        if (y > -2) {
                if (t1 & 8)
                        set1pixel(x1, y1, col);
                if (t1 & 4)
                        set1pixel(x2, y1, col);
                if (t1 & 2)
                        set1pixel(x3, y1, col);
                if (t1 & 1)
                        set1pixel(x4, y1, col);
        }
        y1 += 1;             /* Position of next row */
        if (y > -3) {
                if (t2 & 8)
                        set1pixel(x1, y1, col);
                if (t2 & 4)
                        set1pixel(x2, y1, col);
                if (t2 & 2)
                        set1pixel(x3, y1, col);
                if (t2 & 1)
                        set1pixel(x4, y1, col);
        }
        y1 += 1;             /* Position of next row */
        if (y > -4) {
                if (t3 & 8)
                        set1pixel(x1, y1, col);
                if (t3 & 4)
                        set1pixel(x2, y1, col);
                if (t3 & 2)
                        set1pixel(x3, y1, col);
                if (t3 & 1)
                        set1pixel(x4, y1, col);
        }
}

/*	Term routines
 */

wtext(x,y,text,bold)
char *text;
{
	scmove(x,y);
	if ( bold ) {
		standout();
		addstr(text);
		standend();
	} else
		addstr(text);
}

static setpixels(x,y,xsize,ysize,col)
{
	int yy;
	int xx;

	for ( yy=y; yy < y+ysize; yy++ ) {
		for ( xx=x; xx < x+xsize; xx++ ) {
			set1pixel(xx,yy,col);
		}
	}
}

static int set1pixel(x,y,col) {
	scmove(x,y);
	switch ( col ) {
	case WHITE :
		addstr("  ");
		break;
	case RSIDE :
		addstr("||");
		break;
	case LSIDE :
		addstr("||");
		break;
	case BOTTOM :
		addstr("--");
		break;
	case ULCORN :
	case BLCORN :
		addstr("+-");
		break;
	case URCORN :
	case BRCORN :
		addstr("-+");
		break;
	case SHADOW :
		addstr("==");
		break;
	default :
		standout();
		addstr(blostr);
		standend();
		break;
	}
}

scinsert(y)
{
	scmove(0,y);
	insertln();
    set1pixel(NORM_OFFSET-1, 0, LSIDE);
    set1pixel(NORM_OFFSET+UWIDTH, 0, RSIDE);
}

scdelete(y)
{
	show_score(0);
	show_next(0);
	scmove(0,y);
	deleteln();
}

static scmove(x,y)
{
	move(y*VUNIT,x*HUNIT);
}

static scbeep()
{

	if (bp) beep();
}

