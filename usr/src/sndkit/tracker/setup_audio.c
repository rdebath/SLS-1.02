/* setup_audio.c */
/* higher level interface to the raw metal */

/* $Id: setup_audio.c,v 3.4 1992/11/27 10:29:00 espie Exp espie $
 * $Log: setup_audio.c,v $
 * Revision 3.4  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.3  1992/11/24  10:51:19  espie
 * Added check before closing for the sgi.
 *
 * Revision 3.2  1992/11/23  17:18:59  espie
 * *** empty log message ***
 *
 * Revision 3.1  1992/11/20  14:53:32  espie
 * Added finetune.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "extern.h"

LOCAL char *id = "$Id: setup_audio.c,v 3.4 1992/11/27 10:29:00 espie Exp espie $";


LOCAL BOOL opened = FALSE;
LOCAL int ask_freq, real_freq, oversample;
LOCAL BOOL stereo;

/* setup_audio(frequency, stereo, oversample, sync):
 * try to avoid calling open_audio and other things
 * all the time
 */
void setup_audio(f, s, o, sync)
int f;
BOOL s;
int o;
BOOL sync;
    {

    if (!opened)
        {
        ask_freq = f;
        stereo = s;
        oversample = o;
        real_freq = open_audio(f, s);
        init_player(o, real_freq);
        opened = TRUE;
        }
    else
        {
        int new_freq;

        if (s != stereo || f != ask_freq)
            {
            ask_freq = f;
            stereo = s;
            close_audio();
            new_freq = open_audio(f, s);
            }
        else
            new_freq = real_freq;

        if (new_freq != real_freq || oversample != o)
            {
            real_freq = new_freq;
            oversample = o;
            init_player(o, real_freq);
            }
        }
	set_synchro(sync);
    }

void do_close_audio()
	{
	if (opened)
		close_audio();
	opened = FALSE;
	}

