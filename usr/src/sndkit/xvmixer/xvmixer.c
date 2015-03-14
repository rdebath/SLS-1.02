/*
 * xvmixer.c	xview3 based mixer program for Linux
 *
 *	(c) Hannu Savolainen 1993
 *	    (hsavolai@cs.helsinki.fi)
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/soundcard.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/svrimage.h>
#include "locked.bit"
#include "unlocked.bit"
#include "rec.bit"
 
void		slider_moved();
void		toggle_changed();

int mixer;			/* The mixer device */
int devmask = 0;		/* Bitmask for supported mixer devices */
int recsrc = 0;			/* Currently selected recording sources */
int recmask = 0;		/* Supported recording sources */
int stereodevs = 0;		/* Channels supporting stereo */
int locked     = -1;		/* Channels with left and right locked */
char *labels[SOUND_MIXER_NRDEVICES] = SOUND_DEVICE_LABELS;

int level[SOUND_MIXER_NRDEVICES];
Panel_item	volume_l[SOUND_MIXER_NRDEVICES];
Panel_item	volume_r[SOUND_MIXER_NRDEVICES];
Panel_item	buttons[SOUND_MIXER_NRDEVICES];
int		key;

Server_image locked_image, unlocked_image, rec_image;

int main(argc, argv)
	int	argc;
	char	*argv[];
{
	Frame		frame;
	Panel		panel;
	int		i, n, left, right, x, rec;
	int		stereo;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	  if ((mixer=open("/dev/mixer", O_RDWR, 0)) == -1)
	  {
	     perror("Open: /dev/mixer");
	     exit(-1);
	  }
	
	  if ((devmask=ioctl(mixer, SOUND_MIXER_READ_DEVMASK, 0)) == -1)
	  {
	     perror("SOUND_MIXER_READ_DEVMASK: /dev/mixer");
	     exit(-1);
	  }
	
	  if ((recmask=ioctl(mixer, SOUND_MIXER_READ_RECMASK, 0)) == -1)
	  {
	     perror("SOUND_MIXER_READ_RECMASK: /dev/mixer");
	     exit(-1);
	  }
	
	  if ((recsrc=ioctl(mixer, SOUND_MIXER_READ_RECSRC, 0)) == -1)
	  {
	     perror("SOUND_MIXER_READ_RECSRC: /dev/mixer");
	     exit(-1);
	  }
	
	  if ((stereodevs=ioctl(mixer, SOUND_MIXER_READ_STEREODEVS, 0)) == -1)
	  {
	     perror("SOUND_MIXER_READ_STEREODEVS: /dev/mixer");
	     exit(-1);
	  }

	frame = (Frame) xv_create(XV_NULL, FRAME, 
		FRAME_LABEL, argv[0],
		XV_WIDTH, 800,
		XV_HEIGHT, 400,
		NULL);
 
	panel = (Panel) xv_create(frame, PANEL,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_ITEM_Y_GAP, 0,
		NULL);

	locked_image = (Server_image)xv_create((int)NULL, SERVER_IMAGE,
		XV_HEIGHT,		locked_height,
		XV_WIDTH,		locked_width,
		SERVER_IMAGE_X_BITS,	locked_bits,
		NULL);

	unlocked_image = (Server_image)xv_create((int)NULL, SERVER_IMAGE,
		XV_HEIGHT,		unlocked_height,
		XV_WIDTH,		unlocked_width,
		SERVER_IMAGE_X_BITS,	unlocked_bits,
		NULL);

	rec_image = (Server_image)xv_create((int)NULL, SERVER_IMAGE,
		XV_HEIGHT,		rec_height,
		XV_WIDTH,		rec_width,
		SERVER_IMAGE_X_BITS,	rec_bits,
		NULL);

	key = xv_unique_key();
	n = 0;
	for(i=0; i<SOUND_MIXER_NRDEVICES; i++) 
	if ((1 << i) & devmask) {
	
		if ((level[i] = ioctl(mixer, 
				SOUND_BASE|SOUND_READ|SOUND_MIXER|i, 0)) == -1)
		{
			perror("SyncVolume: /dev/mixer");
			exit(-1);
		}
	
		left = level[i] & 0x7f;
		right = (level[i] >> 8) & 0x7f;

		if ((1 << i) & locked) left = right = (left+right)/2;

		stereo = !!((1 << i) & stereodevs);	/* 0 or 1 */

		rec = !!((1 << i) & recsrc);
		rec <<= 1;

		x = n++ * 50;

		xv_create(panel, PANEL_MESSAGE,
			PANEL_LABEL_STRING, labels[i],
			XV_X, x+4,
			XV_Y, 2,
			NULL);
 
		volume_l[i] = (Panel_item) xv_create(panel, PANEL_SLIDER, 
			XV_KEY_DATA, key, i+1,
			PANEL_NOTIFY_LEVEL, PANEL_ALL,
			PANEL_NOTIFY_PROC, slider_moved,
			PANEL_SHOW_RANGE, FALSE,
			PANEL_SHOW_VALUE, FALSE,
			PANEL_DIRECTION, PANEL_VERTICAL,
			XV_X, x + 4 + !stereo * 7,
			XV_Y, 20,
			PANEL_VALUE, left,
			NULL);
		
		if (stereo) {
			volume_r[i] = (Panel_item) xv_create(panel, PANEL_SLIDER, 
				XV_KEY_DATA, key, -(i+1),
				PANEL_NOTIFY_LEVEL, PANEL_ALL,
				PANEL_NOTIFY_PROC, slider_moved,
				PANEL_SHOW_RANGE, FALSE,
				PANEL_SHOW_VALUE, FALSE,
				PANEL_DIRECTION, PANEL_VERTICAL,
				XV_X, x+15+4,
				XV_Y, 20,
				PANEL_VALUE, right,
				NULL);
		}


		if ((1 << i) & recmask)
		{	/* Possible recording source */
			buttons[i] = (Panel_item) xv_create(panel, PANEL_TOGGLE,
				PANEL_CHOICE_IMAGES, locked_image, rec_image, NULL,
				XV_KEY_DATA, key, i,
				PANEL_NOTIFY_PROC, toggle_changed,
				PANEL_LAYOUT, PANEL_VERTICAL,
				XV_Y, 140,
				XV_X, x,
				PANEL_VALUE, rec | 0x01,
				NULL);
		} else {
			buttons[i] = (Panel_item) xv_create(panel, PANEL_TOGGLE,
				PANEL_CHOICE_IMAGES, locked_image, NULL,
				XV_KEY_DATA, key, i,
				PANEL_NOTIFY_PROC, toggle_changed,
				PANEL_LAYOUT, PANEL_VERTICAL,
				XV_Y, 140,
				XV_X, x,
				PANEL_VALUE, rec | 0x01,
				NULL);
		}
	}

	window_fit(panel);

	window_fit(frame);
	xv_main_loop(frame); 

	exit(0);
} 
 

void
slider_moved(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	int dev, ch, val;
	
	dev = (int) xv_get(item, XV_KEY_DATA, key);
	val = value & 0x7f;

	if (dev>0)
	   { 
		ch = 0;
		dev--;
	   }
	else
	   {
		ch = 1;
		dev = -dev - 1;
	   }

	if ((1 << dev) & locked)
	   level[dev] = ((val << 8) & 0x7f00) | (val & 0x007f);
	else
	  if (ch)
	     level[dev] = (level[dev] & 0x007f) | (val << 8);
	  else
	     level[dev] = (level[dev] & 0x7f00) | val;

	if (ioctl(mixer, SOUND_BASE|SOUND_MIXER|SOUND_WRITE|dev, 
		level[dev]) == -1)
	{
		perror("Set mixer: /dev/mixer");
		exit(-1);
	}

	if ((1 << dev) & stereodevs)
	if ((1 << dev) & locked)	/* Sync the other channel */
	if (ch) {
		xv_set(volume_l[dev], PANEL_VALUE, val, NULL);
	} else {
		xv_set(volume_r[dev], PANEL_VALUE, val, NULL);
	}
}

void
toggle_changed(Panel_item item, int value, Event *event)
{
	int i, dev, rec, lock, val, tmp_src, changed;
	dev = (int) xv_get(item, XV_KEY_DATA, key);

	lock = value & 0x01;
	rec  = (value & 0x02) >> 1;

	if (locked ^ (lock << dev))	/* Locked or unlocked */
	if (lock) {
		locked |= (1 << dev);

		val = ((level[dev] & 0x7f00) >> 8) + (level[dev] & 0x007f);
		val /= 2;
	   	level[dev] = ((val << 8) & 0x7f00) | (val & 0x007f);

		if (ioctl(mixer, SOUND_BASE|SOUND_MIXER|SOUND_WRITE|dev, 
			level[dev]) == -1)
		{
			perror("Set mixer: /dev/mixer");
			exit(-1);
		}

		xv_set(volume_l[dev], PANEL_VALUE, val, NULL);

		if ((1 << dev) & recmask)
		xv_set(buttons[dev], PANEL_CHOICE_IMAGES,
			locked_image, rec_image, NULL, 
			NULL);
		else
		xv_set(buttons[dev], PANEL_CHOICE_IMAGES,
			locked_image, NULL, 
			NULL);

		if ((1 << dev) & stereodevs)
		   xv_set(volume_r[dev], PANEL_VALUE, val, NULL);

	} else {
		locked &= ~(1 << dev);

		if ((1 << dev) & recmask)
		xv_set(buttons[dev], PANEL_CHOICE_IMAGES,
			unlocked_image, rec_image, NULL, 
			NULL);
		else
		xv_set(buttons[dev], PANEL_CHOICE_IMAGES,
			unlocked_image, NULL, 
			NULL);
	}

	if (recsrc ^ (rec << dev))	/* Rec/play changed */
	{
	  recsrc ^= (1 << dev);
	  tmp_src = recsrc;

	  if ((recsrc=ioctl(mixer, SOUND_MIXER_WRITE_RECSRC, recsrc)) == -1)
	  {
	     perror("SOUND_MIXER_WRITE_RECSRC: /dev/mixer");
	     exit(-1);
	  }

	  if (recsrc ^ tmp_src)	/* The device has changed the mask */
	  {
	  	changed = recsrc ^ tmp_src;

	  	for (i=0;i<SOUND_MIXER_NRDEVICES;i++)
	  	if (changed & (1 << i))
	  	{
	  		xv_set(buttons[i], PANEL_VALUE, 
	  			(int)xv_get(buttons[i], PANEL_VALUE) ^ 0x02,
	  			NULL);
	  	}
	  }
	}
}
