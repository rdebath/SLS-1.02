/*

This file contains just a stub version of the Sound Driver.

The real version is available at tsx-11.mit.edu
(pub/linux/ALPHA/sound/snd-driv-0.x.tar.Z)

Hannu Savolainen
hsavolai@cs.helsinki.fi
*/

long soundcard_init(long mem_start)
{
	return mem_start;
}

void sound_mem_init(void)
{
}

#ifdef CONFIG_SOUND
#error The Sound Driver not installed.
#endif
