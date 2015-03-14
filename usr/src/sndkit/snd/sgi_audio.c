/* sgi_audio.c */

/* $Id: sgi_audio.c,v 3.4 1992/11/27 10:29:00 espie Exp espie $
 * $Log: sgi_audio.c,v $
 * Revision 3.4  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.3  1992/11/24  10:51:19  espie
 * Added pseudo discardbuffer.
 *
 * Revision 3.2  1992/11/22  17:20:01  espie
 * Checks for finetune ?
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 2.11  1992/11/17  15:38:00  espie
 * Dummy discard_buffer()
 * Changed sync_audio value again.
 * Added synchro for dump.
 * Bug fix: must ask new frequency after we tried to set it to get it
 * rounded up.
 * Added stereo option (kind of).
 * Separated mix/stereo stuff.
 * Checked buffer size.
 * Added possibility to get back to MONO for the sgi.
 * Added stereo capabilities to the indigo version.
 * Ask the frequency to the audio device.
 * Corrected bug: when closing audio,
 * we now wait for the samples queue to be empty.
 */

#include <audio.h>
#include <malloc.h>
#include <stdio.h>
#include "defs.h"
#include "extern.h"

X int sginap(long ticks);
     
LOCAL char *id = "$Id: sgi_audio.c,v 3.4 1992/11/27 10:29:00 espie Exp espie $";

LOCAL signed short *buffer;
LOCAL int index;

LOCAL int number;
LOCAL BOOL sync = FALSE;

LOCAL ALport audio;
LOCAL ALconfig config;

LOCAL BOOL donotwait = FALSE;
LOCAL long chpars[] = {AL_OUTPUT_RATE, 0};

LOCAL int stereo;  /* are we playing stereo or not ? */
/* 256th of primary/secondary source for that side. */
LOCAL int primary, secondary;

void set_mix(percent)
int percent;
    {
    percent *= 256;
    percent /= 100;
    primary = percent;
    secondary = 512 - percent;
    }

int open_audio(f, s)
int f, s;
    {

	donotwait = FALSE;
    chpars[1] = f;
    if (f != 0)
        ALsetparams(AL_DEFAULT_DEVICE, chpars, 2);
    ALgetparams(AL_DEFAULT_DEVICE, chpars, 2);
    config = ALnewconfig();
    stereo = s;
    if (stereo)
        {
        ALsetchannels(config, AL_STEREO);
        number = 2;
        set_mix(30);
        }
    else
        {
        ALsetchannels(config, AL_MONO);
        number = 1;
        }
    ALsetwidth(config, AL_SAMPLE_16);
    audio = ALopenport("soundtracker mono", "w", config);
    index = 0;
    buffer = malloc(sizeof(signed short) * number * chpars[1]);
    return chpars[1];
    }

void set_synchro(s)
BOOL s;
	{
	sync = s;
	}

int update_frequency()
	{
	int oldfreq;

	oldfreq = chpars[1];
	ALgetparams(AL_DEFAULT_DEVICE, chpars, 2);
	if (chpars[1] != oldfreq)
		{
		buffer = realloc(buffer, sizeof(signed short) * number * chpars[1]);
		return chpars[1];
		}
	else
		return 0;
	}


void output_samples(int left, int right)
    {
    if (stereo)
        {
        buffer[index++] = (left * primary + right * secondary)/256;
        buffer[index++] = (right * primary + left * secondary)/256;
        }
    else
        buffer[index++] = left + right;
    }

void flush_buffer(void)
    {
    ALwritesamps(audio, buffer, index);
	if (sync)
		while(ALgetfilled(audio) > index * 10)
			/* busy wait */
			;
    index = 0;
    }

void discard_buffer(void)
	{
	donotwait = TRUE;
	/* mostly not implemented, only working when using close_audio
	 * right after
	 */
	}

void close_audio(void)
    {
	if (!donotwait)
		{
		while(ALgetfilled(audio) != 0)
			sginap(1);
		}
    ALcloseport(audio);
    ALfreeconfig(config);
    free(buffer);
    }

