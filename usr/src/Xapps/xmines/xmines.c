#include <sys/time.h>
#include <signal.h>
#include <fcntl.h>
#include <X11/Xlib.h>
#include "xmines.h"

#define Neighbors(func, x, y)   func(x-1,y-1);func(x,y-1);func(x+1,y-1);\
                                func(x-1,y  );            func(x+1,y  );\
                                func(x-1,y+1);func(x,y+1);func(x+1,y+1)


void fillBlanks();

#define AWIDTH  10
#define AHEIGHT 10

#define MISSMARKED 10
#define BOOM 11
/*
    bit 5   : Marked
    bit 4   : Cover/uncovered
    bit 3-0 : number of mines in adjoining cells
*/
  
int minefield[50][20];
int numcells = 0;
int nummines = 10;
int numuncovered = 0;
int nummarked;

int done;
int interval;
int height;
int width;

int start_time;

void ticktock()
{
}

delay(time)
int time;
{
    struct itimerval value,ovalue;

    signal(SIGALRM,ticktock);
    value.it_interval.tv_sec = time/1000;
    value.it_interval.tv_usec = (time %1000)*1000;
    value.it_value.tv_sec = time/1000;
    value.it_value.tv_usec = (time %1000)*1000;
    setitimer(ITIMER_REAL,&value,&ovalue);
    pause();
    value.it_interval.tv_sec = 0;
    value.it_interval.tv_usec = 0;
    value.it_value.tv_sec = 0;
    value.it_value.tv_usec = 0;
    setitimer(ITIMER_REAL,&value,&ovalue);
}


setdoneflag(i)
{
    done = i;
}


setup_signals()
{
     int flags;
     extern Display *display;
     
     signal(SIGIO, ticktock);

     flags = fcntl(ConnectionNumber(display), F_GETFL, 0);
/*     flags |= FASYNC;   */
     fcntl(ConnectionNumber(display), F_SETFL, flags);
     fcntl(ConnectionNumber(display), F_SETOWN, getpid());
}

main(argc,argv)
    int argc;
    char **argv;
{
    openScores();

    parseOptions(argc,argv);
    nummines = 99;
    height = 16;
    width = 30;
    XStuff(argc,argv);

    srandom(getpid());
    setup_signals();
    
    while(1)
    {
        nummarked = 0;
        numcells = height*width;
        numuncovered = 0;
        newscreen();
        draw_screen();
        done = 0;
	start_time = 0;
        while(!done)
        {
	    CheckforEvent();
            if(numuncovered)
            {
		if (start_time == 0) start_time = time(0);
		show_score(0);
            }
	    delay(100);
        }
        if(done == 1)
        {
            newScore(getscore());
        }
        else
        {
            showallmines();
        }
        show_high_scores(0);
    }
}

getscore()
{
    static int frozen = -1;
    if (done) {
	 if (frozen < 0) {
	      frozen = (start_time ? time(0) - start_time : 0);
	 }
	 return frozen;
    }
    else {
	 frozen = -1;
	 return (start_time ? time(0) - start_time : 0);
    }
}

getminesleft()
{
    return nummines - nummarked;
}


cntmines(x,y)
{
    return IsMine(x-1,y-1) + IsMine(x-1,y) + IsMine(x-1,y+1) +
        IsMine(x,y-1) + IsMine(x,y+1) +
        IsMine(x+1,y-1) + IsMine(x+1,y) + IsMine(x+1,y+1);
}

Number(x,y)
{
    return minefield[x][y] & 0xf;
}

MissmarkCell(x,y)
{
    minefield[x][y] = MISSMARKED;
}

MarkCell(x,y)
{
    if(InArray(x,y) && !IsVisible(x,y))
    {
        if(IsMarked(x,y))
        {
            minefield[x][y] &= ~MARKED;
            nummarked--;
        }
        else
        {
            minefield[x][y] |= MARKED;
            nummarked++;
        }
    }
    DrawCell(x,y);
}

BoomCell(x,y)
{
    minefield[x][y] = BOOM;
    DrawCell(x,y);
}

UncoverCell(x,y)
{
    minefield[x][y] &= 0xf;
    numuncovered++;
}

WriteCell(x,y,val)
{
    minefield[x][y] = val;
}

ReadCell(x,y)
{
    return minefield[x][y];
}

newscreen()
{
    int i,j,k;
    int x,y;

    for(i=0; i < width; i++)
    {
        for(j=0; j < height; j++)
        {
            WriteCell(i,j,0);
        }
    }
    for(k=0; k < nummines; k++)
    {
        do
        {
            x = random() % width;
            y = random() % height;
        } while(IsMine(x,y));
        WriteCell(x,y,MINE + COVERED);
    }
    for(i=0; i < width; i++)
    {
        for(j=0; j < height; j++)
        {
            if(!IsMine(i,j))
            {
                WriteCell(i,j,COVERED + cntmines(i,j));
            }
        }
    }
}

showallmines()
{
    int x,y;

    for(x=0; x < width; x++)
    {
        for(y=0; y < height; y++)
        {
            if(!IsVisible(x,y))
            {
                if(!IsMine(x,y) && IsMarked(x,y))
                {
                    MissmarkCell(x,y);
                    DrawCell(x,y);
                }
                if(IsMine(x,y) && !IsMarked(x,y))
                {
                    UncoverCell(x,y);
                    DrawCell(x,y);
                }
            }
        }
    }
}


Highlight(x,y)
{
    if(!IsMarked(x,y) && !IsVisible(x,y))
    {
        DrawHighlight(x,y);
    }
}

unHighlight(x,y)
{
    if(!IsMarked(x,y) && !IsVisible(x,y))
    {
        DrawCell(x,y);
    }
}


void Show(x,y)
    int x,y;
{
    if(InArray(x,y) && !IsVisible(x,y) && !IsMarked(x,y))
    {
        if(IsMine(x,y))
        {
            BoomCell(x,y);
            DrawCell(x,y);
            setdoneflag(-1);
        }
        else
        {
            UncoverCell(x,y);
            fillBlanks(x,y);
            DrawCell(x,y);
            if(numuncovered == numcells - nummines)
            {
                setdoneflag(1);
            }
        }
    }
}

/* fill blanks starting at x, y */
void
fillBlanks( x, y )
int x,y;
{
    if (IsBlank (x, y)) {
	Neighbors (Show, x, y);
    }
}

/* highlight neighbors which are invisible (two button down) */
void
highlightInvisible(x,y)
int x, y;
{
    Highlight(x,y);
    Neighbors( Highlight, x, y);
}

unhighlightInvisible(x,y)
    int x, y;
{
    unHighlight(x,y);
    Neighbors( unHighlight, x, y);
}


int
foundMines(x,y)
int x,y;
{
    int     visibleMineCount;

    visibleMineCount = 0;

    Neighbors (visibleMineCount += IsMarked, x, y);

    return visibleMineCount;
}


/* fill numbers if all mines are already discovered (two button up) */
void
fillNumbers( x, y )
int x,y;
{
    if(IsVisible(x, y))
    {
	if (foundMines (x, y) == Number (x, y))
        {
	    Neighbors (Show, x, y);
        }
    }
    unhighlightInvisible(x,y);
}

