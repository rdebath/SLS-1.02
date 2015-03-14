/*
   mgrload - show cpu load average in an mgr window.  Requires accompanying
	     loadavgd daemon for sysinfo data extraction from kernel.

	     Mark Dapoz  90/06/21
		mdapoz@hybrid.UUCP or 
		mdapoz%hybrid@cs.toronto.edu
*/

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/utsname.h>
#include "term.h"

#ifndef	SAMPLES
#define	SAMPLES		120	/* number of samples to keep */
#endif
#ifndef	INTERVAL
#define	INTERVAL	60	/* time interval (sec) between samples*/
#endif
#ifndef	PSIZE
#define	PSIZE		50	/* size of one display partition; .50 proc */
#endif

#define	XHOME		20	/* default location and size of display */
#define	YHOME		295
#define	XSIZE		300
#define	YSIZE		50

#define	CONTEXT P_POSITION | P_WINDOW | P_FLAGS | P_EVENT | P_CURSOR | P_MENU

int xscale, yscale, rscale;
int xmin, xmax, ymin, ymax;
int width;
char *version = "Version 1.0  90/06/21";

main(argc,argv)
int argc;
char **argv;
{
	int xhome=XHOME, yhome=YHOME; /* window position */
	int xsize=XSIZE, ysize=YSIZE; /* window size */
	int erropt=0;
	int samples[SAMPLES];		/* circular queue of samples */
	int head=0;			/* queue pointer */
	int partitions, last_part=0;
	char event[80], c;		/* mgr event queue */
	int timer_event(), done();	/* mgr event handlers */
	register int i;
	int shm;			/* sysinfo shared segment */
	double *shmseg;
	char *options, scale[16], stats[16*3], 
	     hostname[sizeof(struct utsname)+32];
	struct utsname unixname;
	extern char *optarg;

	ckmgrterm(*argv);	/* check if an mgr terminal */
				/* process command line options */
	while ((c=getopt(argc, argv, "x:y:l:w:")) != EOF)
	    switch (c) {
		case 'x':	/* x co-ordinate location */
			if (strspn(optarg, "0123456789") == strlen(optarg))
			    xhome=atoi(optarg);
			else
			    erropt++;
			break;
		case 'y':	/* y co-ordinate location */
			if (strspn(optarg, "0123456789") == strlen(optarg))
			    yhome=atoi(optarg);
			else
			    erropt++;
			break;
		case 'l':	/* window length */
			if (strspn(optarg, "0123456789") == strlen(optarg))
			    xsize=atoi(optarg);
			else
			    erropt++;
			break;
		case 'w':	/* window width */
			if (strspn(optarg, "0123456789") == strlen(optarg))
			    xsize=atoi(optarg);
			else
			    erropt++;
			break;
		case '?':	/* unknown option */
			erropt++;
	    }
	if (erropt) {
	    fprintf(stderr, "usage: %s [-x nn] [-y nn] [-l nn] [-w nn]\n",
		argv[0]);
	    exit(1);
	}
	memset(samples, 0, SAMPLES);
	xmin = 0;	/* mgr virtual window size */
	ymin = 0;
	xmax = 999;
	ymax = 999;
	xscale = xmax-xmin;
	yscale = ymax-ymin;
	width=xscale/SAMPLES; /* width of one bar */

		/* find loadavgd segment */
	if ((shm = shmget(ftok("/unix",'a'),12,0)) < 0) {
	    perror("unable to attach sysinfo shared segment");
	    exit(1);
	}
	if (uname(&unixname) < 0) { /* get system info */
	    perror("unable to get system name");
	    exit(1);
	}
	m_setup(0);		/* init mgr */
	m_push(CONTEXT); /* save window settings */
	m_setcursor(CS_INVIS); /* get rid of cursor */
	m_ttyset(); /* setup communication channel */
				/* setup system interrupts */
	signal(SIGALRM, timer_event); /* sampling interrupt */
	signal(SIGTERM, done); /* exit */
	signal(SIGINT, done); /* exit */
	signal(SIGQUIT, done); /* exit */
	signal(SIGHUP, done); /* exit */
				/* setup mgr events */
	m_setevent(ACTIVATE,  "A\r");
	m_setevent(COVERED,   "C\r");
	m_setevent(UNCOVERED, "U\r");
	m_setevent(RESHAPE,   "H\r");
	m_setevent(REDRAW,    "R\r");

	m_shapewindow(xhome, yhome, xsize, ysize); /* resize window */
	m_func(B_SET);		/* set drawing mode on */
	m_clear(); m_flush();	/* clear screen */
	m_sendme("S\r"); /* sample load average event */
				/* setup menus */
	options="|stats ->|scale ->|refresh|reset|||R\r|X\r|";
	m_loadmenu(1, options); /* top level options */
	sprintf(scale, "|top = %3.2f||", 0.0); /* current scale */
	m_loadmenu(2, scale); /* load second level menu */
	m_linkmenu(1, 1, 2, MF_AUTO); /* link to main menu */
	sprintf(stats, "|max = %3.2f|min = %3.2f||", 0.0, 0.0);
	sprintf(stats,"|max  = %3.2f|min  = %3.2f|last = %3.2f||||",
		 0.0, 0.0, 0.0);
	m_loadmenu(3, stats); 
	m_linkmenu(1, 0, 3, MF_AUTO); /* link stats to main menu */
	m_selectmenu2(1); /* bind menu to right button */
	sprintf(hostname, "|%s|%s %s %s %s|mgrload %s||||", unixname.nodename,
	    unixname.sysname, unixname.release, unixname.version,
	    unixname.machine, version);
	m_loadmenu(10, hostname);
	m_selectmenu(10); /* bind host info to middle button */
	for(;;) {
	    if (m_gets(event) == NULL)
		if (errno = EINTR) /* restart interrupted call */
		    continue;
		else
		    break; /* exit */
	    switch (*event) {
		case 'A':	/* window activated */
		case 'C':	/* window covered */
		case 'U':	/* window uncovered */
		    break;
		case 'X':	/* reset stored samples */
		    memset(samples, 0, SAMPLES);
		    head=0;
		    m_sendme("R\rS\r"); /* force redraw and sample */
		    break;
		case 'R':	/* redraw window */
		case 'H':	/* reshape window */
		    m_func(B_SET);
		    m_clear();
		    redraw(samples, head-1 < 0 ? SAMPLES-1 : head-1);
		    draw_scale(partitions, 0, 999);
		    break;
		case 'S':	/* get load average */
		    shmseg = (double *)shmat(shm, (char *) 0, SHM_RDONLY);
		    samples[head]=(int)(shmseg[0]*100); /* 1 min avg */
		    shmdt(shmseg);
		    partitions=max(samples)/PSIZE+1;
		    sprintf(stats,"|max  = %3.2f|min  = %3.2f|last = %3.2f||||",
			max(samples)/100.0, min(samples)/100.0, 
			samples[head]/100.0);
		    m_loadmenu(3, stats); /* update pop up menu info */
		    rscale=partitions*PSIZE;
		    if (last_part == partitions) { /* fit on last scale? */
			m_func(B_COPY);	/* set copy mode */
/*
			m_bitcopy(width, 0, 1000-width, 1000, 0, 0);
*/
			m_bitcopy(width, 0, 1000, 1000, 0, 0);
			m_func(B_CLEAR);	/* set clear mode */
			m_bitcopy(0, 0, width, 1000, 0, 0);/* clear a column*/
			m_func(B_SET);	/* set drawing mode on */
			for (i=0; i < width; i++) /* draw bar */
			    m_line(i,ymax, i, ymax-samples[head]*yscale/rscale);
			draw_scale(partitions, 0, width-1);
		    } else { /* change scale */
			sprintf(scale, "|top = %3.2f||",partitions*PSIZE/100.0);
			m_loadmenu(2, scale); /* update pop up menu info */
			last_part=partitions;
			m_clear();
			redraw(samples, head);
			draw_scale(partitions, 0, 999);
		    }
		    m_flush();
		    head++;head%=SAMPLES;
		    alarm(INTERVAL);
		    break;
		default:
		    break;
	    }
	}
}

int max(nums) /* find maximum load avarage in queue */
int *nums;
{
	static int i,j;

	for (i=j=0; i < SAMPLES; nums++, i++)
	    j=*nums > j ? *nums : j;
	return(j);
}

int min(nums) /* find minimum load avarage in queue */
int *nums;
{
	static int i,j;

	for (i=0,j=99999; i < SAMPLES; nums++, i++)
	    j=*nums && *nums < j ? *nums : j; /* ignore zero values */
	return(j);
}

draw_scale(sections, left, right) /* draw scale lines on the graph */
int sections;
int left,right;
{
	int i,j;

	m_func(B_XOR);	/* invert pixels for scale */
	for (i=1; i < sections; i++) { /* draw scale lines */
	    j=ymax-i*PSIZE*yscale/rscale;
	    m_line(left, j, right, j);
	}
	m_flush();
}

redraw(nums, head) /* redraw graph from history */
int *nums;
int head;
{
	register int i,j,p;

	m_func(B_SET);	/* draw on */
	for (p=SAMPLES-1, j=head+1; p >= 0; p--) {
	    j=j%SAMPLES; /* wrap? */
	    if (nums[j]) /* a value? */
		for (i=0; i < width; i++) /* draw bar */
		    m_line(p*width+i, ymax, 
			   p*width+i, ymax-nums[j]*yscale/rscale);
	    j++;
	}
}

timer_event() /* cause the load average to be sampled */
{
    m_sendme("S\r"); /* sample load average event */
    signal(SIGALRM, timer_event);
}

done() /* general purpose exit */
{
    m_ttyreset(); /* reset communication channel */
    m_popall(); /* restore window */
    exit(0);
}
