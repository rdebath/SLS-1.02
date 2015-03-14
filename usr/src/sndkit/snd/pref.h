/* pref.h */

/* $Id: pref.h,v 3.2 1992/11/24 10:51:19 espie Exp espie $
 * $Log: pref.h,v $
 * Revision 3.2  1992/11/24  10:51:19  espie
 * Added show and sync.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 2.7  1992/11/17  15:38:00  espie
 * Added dump_song.
 * imask, bcdvol.
 */

struct pref
    {
    int type, speed, tolerate, repeats;
    unsigned long imask;
    int bcdvol;
	int dump_song;
	BOOL show;
	BOOL sync;
    };

