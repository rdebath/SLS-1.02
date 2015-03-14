/* ss10_audio.h */

/* $Id: ss10_audio.c,v 3.7 1993/01/06 17:58:39 espie Exp espie $
 * $Log: ss10_audio.c,v $
 * Revision 3.7  1993/01/06  17:58:39  espie
 * *** empty log message ***
 *
 * Revision 3.6  1992/12/03  15:00:50  espie
 * restore stty.
 *
 * Revision 3.5  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.4  1992/11/24  10:51:19  espie
 * Sync pseudo call.
 *
 * Revision 3.3  1992/11/22  17:20:01  espie
 * Added update_frequency call, mostly unchecked
 *
 * Revision 3.2  1992/11/20  14:53:32  espie
 * Added finetune.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 1.3  1992/11/17  15:38:00  espie
 * discard_buffer() call for snappier interface calls.
 * - Unified support for all sparcs.
 * - moved down to level 2 io.
 */

#include <stdio.h>
#include "defs.h"
#include "extern.h"
#include <sun/audioio.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <stropts.h>
#include <malloc.h>
     
/* things that aren't defined in all sun/audioio.h */

#ifndef AUDIO_ENCODING_LINEAR
#define AUDIO_ENCODING_LINEAR (3)
#endif
#ifndef AUDIO_GETDEV
#define AUDIO_GETDEV	_IOR(A, 4, int)
#endif
#ifndef AUDIO_DEV_UNKNOWN
#define AUDIO_DEV_UNKNOWN (0)
#endif
#ifndef AUDIO_DEV_AMD
#define AUDIO_DEV_AMD (1)
#endif

LOCAL char *id = "$Id: ss10_audio.c,v 3.7 1993/01/06 17:58:39 espie Exp espie $";

LOCAL int audio;

LOCAL struct audio_info info;
LOCAL char *buffer;
LOCAL short *sbuffer;
LOCAL int index;
LOCAL int dsize;

LOCAL int stereo;
LOCAL int primary, secondary;

void set_mix(percent)
int percent;
    {
	percent *= 256;
	percent /= 100;
	primary = percent;
	secondary = 512 - percent;
    }

#define abs(x) ((x) < 0 ? -(x) : (x))

LOCAL int available(f)
int f;
	{
	static int possible[] = { 8000, 9600, 11025, 16000, 18900, 22050, 32000,
		37800, 44100, 48000, 0};
	int best = 0;
	int i;

	for (i = 0; possible[i]; i++)
		if (abs(possible[i] - f) < abs(best - f))
			best = possible[i];
	return best;
	}

int open_audio(f, s)
int f;
int s;
    {
	int type;

    audio = open("/dev/audio", O_WRONLY|O_NDELAY);
    if (audio == -1)
        {
        fprintf(stderr, "Error: could not open audio\n");
        end_all();
        }
	if (f == 0)
		f = 22050;
		/* round frequency to acceptable value */
	f = available(f);

		/* check whether we know about AUDIO_ENCODING_LINEAR */
	if (ioctl(audio, AUDIO_GETDEV, &type) ||
	type == AUDIO_DEV_UNKNOWN || type == AUDIO_DEV_AMD)
		{
			/* not a ss 10 -> revert to base quality audio */
		stereo = 0;
		dsize = 1;
		info.play.sample_rate = 8000;
		info.play.encoding = AUDIO_ENCODING_ULAW;
		info.play.channels = 1;
		}
	else
		{
			/* tentative set up */
		stereo = s;
		AUDIO_INITINFO(&info);
		info.play.sample_rate = f;
		info.play.precision = 16;
		dsize = 2;
		if (stereo)
			{
			info.play.channels = 2;
			set_mix(30);
			}
		else
			info.play.channels = 1;
			/* try it */
		info.play.encoding = AUDIO_ENCODING_LINEAR;
		if (ioctl(audio, AUDIO_SETINFO, &info) != 0)
			/* didn't work: fatal problem */
			end_all();
		}
	index = 0;
	buffer = (char *)malloc(dsize * info.play.channels * info.play.sample_rate);
	sbuffer = (short *) buffer;
	if (!buffer)
		end_all();
	return info.play.sample_rate;
    }

void set_synchro(s)
BOOL s;
	{
	}

int update_frequency()
	{
	int oldfreq;

	oldfreq = info.play.sample_rate;
	if (ioctl(audio, AUDIO_GETINFO, &info) == 0)
		{
		if (oldfreq != info.play.sample_rate)
			{
			buffer = realloc(buffer, 
				dsize * info.play.channels * info.play.sample_rate);
			sbuffer = (short *)buffer;
			return info.play.sample_rate;
			}
		}
	return 0;
	}


LOCAL int sign(x)
unsigned char x;
    {
    return x;
    }

/************************************************************************/
/*      For routine 'cvt' only                                          */
/************************************************************************/
/*      Copyright 1989 by Rich Gopstein and Harris Corporation          */
/************************************************************************/

LOCAL unsigned int cvt(ch)
int ch;
    {
    int mask;

    if (ch < 0)
        {
        ch = -ch;
        mask = 0x7f;
        }
    else
        mask = 0xff;

    if (ch < 32)
        {
        ch = 0xF0 | 15 - (ch / 2);
        }
    else if (ch < 96)
        {
        ch = 0xE0 | 15 - (ch - 32) / 4;
        }
    else if (ch < 224)
        {
        ch = 0xD0 | 15 - (ch - 96) / 8;
        }
    else if (ch < 480)
        {
        ch = 0xC0 | 15 - (ch - 224) / 16;
        }
    else if (ch < 992)
        {
        ch = 0xB0 | 15 - (ch - 480) / 32;
        }
    else if (ch < 2016)
        {
        ch = 0xA0 | 15 - (ch - 992) / 64;
        }
    else if (ch < 4064)
        {
        ch = 0x90 | 15 - (ch - 2016) / 128;
        }
    else if (ch < 8160)
        {
        ch = 0x80 | 15 - (ch - 4064) /  256;
        }
    else
        {
        ch = 0x80;
        }
    return (mask & ch);
    }


void output_samples(left, right)
int left, right;
    {
	if (stereo)
		{
		sbuffer[index++] = (left * primary + right * secondary)/256;
		sbuffer[index++] = (right * primary + left * secondary)/256;
		}
	else
		switch(info.play.encoding)
			{
		case AUDIO_ENCODING_LINEAR:
			sbuffer[index++] = left + right;
			break;
		case AUDIO_ENCODING_ULAW:
			buffer[index++] = cvt((left + right) /16);
			break;
			}
    }

void flush_buffer()
    {
	write(audio, buffer, dsize * index);
	index = 0;
    }

void discard_buffer()
	{
	ioctl(audio, I_FLUSH, FLUSHW);
	}

void close_audio()
    {
	free(buffer);
    close(audio);
    }

