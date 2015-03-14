#ifndef SOUNDCARD_H
#define SOUNDCARD_H
/*
 *	linux/soundcard.h
 *
 *	Sound Card driver for Linux
 *
 *	(C) Hannu Savolainen 1992
 *
 */

/*
 *	Supported card ID numbers
 */

#define SNDCARD_ADLIB	1
#define SNDCARD_SB	2
#define SNDCARD_PAS	3

/*
 * IOCTL Commands. Suffix 0x4252 is string "SB".
 */

#define SNDCTL_CONFIGURE				0x00014252

#define SNDCTL_FM_LOAD_INSTR				0x01014252
/* #define SNDCTL_FM_RETURN_INSTR			0x01034252	*/
#define SNDCTL_SEQ_SYNC					0x01044252
#define SNDCTL_SEQ_RESET				0x01054252
#define SNDCTL_SYNTH_INFO				0x01064252
#define SNDCTL_SEQ_TESTMIDI				0x01074252
#define SNDCTL_SEQ_PERCMODE				0x01084252

#define SNDCTL_DSP_SPEED				0x01114252
#define SNDCTL_DSP_STEREO				0x01124252
#define SNDCTL_DSP_GETBLKSIZE				0x01134252
#define SNDCTL_DSP_SYNC					0x01144252
#define SNDCTL_DSP_RESET				0x01154252
#define SNDCTL_DSP_SAMPLESIZE				0x01164252
/* 8, 12 or 16)	*/

#define SEQ_FMNOTEOFF		0
#define SEQ_FMNOTEON		1
#define SEQ_WAIT		2
#define SEQ_FMPGMCHANGE		3
#define SEQ_SYNCTIMER		4
#define SEQ_MIDIPUTC		5
#define SEQ_DRUMON		6	/* Play percussive instrument */
#define SEQ_DRUMOFF		7

typedef unsigned char sbi_instr_data[16];

struct sbi_instrument {
		int 		channel;	/*	Channel to be programmed 	*/
		sbi_instr_data	operators;	/*	Register settings for operator cells (.SBI format)	*/
	};

struct synth_info {	/* Read only */
		int	synth_type;
#define SYNTH_TYPE_FM			0

		int	synth_subtype;
#define FM_TYPE_ADLIB			0
#define FM_TYPE_OPL3			1

		int	perc_mode;	/* 0=off 1=off */
		int	nr_voices;
		int	nr_drums;
		int	instr_bank_size;
		int	dummies[20];	/* Reserve space */
	};
/*
	Definitions for a "universal" sound driver 
	by Craig metz (cmetz@thor.tjhsst.edu)
*/	
	
/*  
	IOCTL requests take the general form of a base address plus
	a device type plus a request type. The base address fills the
	top three bytes of the request longword and is formed from the 
	letters 'SND'. The top nybble of the remaining byte indicates
	the device type to be interacted with, and the request type
	indicates what the device needs to do, and bit 3 of the lower
	nybble distinguishes read and write. 

	IOCTL Calling Form:

		ioctl(fh, SOUND_call, &parameter);
*/

#define SOUND_BASE	0x534E4400

#define SOUND_READ	0x00000000
#define SOUND_WRITE	0x00000008

/* 
	Mixer control - device type 0 

	All parameters are of type "unsigned short int" - The LSB is the 
	left value, the MSB is the right value. Both are in percents, 0 
	being mute and 100 being full power. In the event that the card 
	only supports mono mixer control, the LSB will be the value used.
*/

#define SOUND_MIXER		0x00
#define SOUND_MIXER_TYPE	unsigned short int

#define SOUND_MIXER_VOLUME	0x0
#define SOUND_MIXER_BASS	0x1
#define SOUND_MIXER_TREBLE	0x2
#define SOUND_MIXER_MIDI	0x3
#define SOUND_MIXER_PCM		0x4
#define SOUND_MIXER_SPEAKER	0x5
#define SOUND_MIXER_LINE	0x6
#define SOUND_MIXER_MIC		0x7

#define SOUND_MIXER_READ_VOLUME		(SOUND_BASE | SOUND_MIXER | SOUND_READ  | SOUND_MIXER_VOLUME )
#define SOUND_MIXER_READ_BASS		(SOUND_BASE | SOUND_MIXER | SOUND_READ  | SOUND_MIXER_BASS   )
#define SOUND_MIXER_READ_TREBLE		(SOUND_BASE | SOUND_MIXER | SOUND_READ  | SOUND_MIXER_TREBLE )
#define SOUND_MIXER_READ_MIDI		(SOUND_BASE | SOUND_MIXER | SOUND_READ  | SOUND_MIXER_MIDI   )
#define SOUND_MIXER_READ_PCM		(SOUND_BASE | SOUND_MIXER | SOUND_READ  | SOUND_MIXER_PCM    )
#define SOUND_MIXER_READ_SPEAKER	(SOUND_BASE | SOUND_MIXER | SOUND_READ  | SOUND_MIXER_SPEAKER)
#define SOUND_MIXER_READ_LINE		(SOUND_BASE | SOUND_MIXER | SOUND_READ  | SOUND_MIXER_LINE   )
#define SOUND_MIXER_READ_MIC		(SOUND_BASE | SOUND_MIXER | SOUND_READ  | SOUND_MIXER_MIC    )

#define SOUND_MIXER_WRITE_VOLUME	(SOUND_BASE | SOUND_MIXER | SOUND_WRITE | SOUND_MIXER_VOLUME )
#define SOUND_MIXER_WRITE_BASS		(SOUND_BASE | SOUND_MIXER | SOUND_WRITE | SOUND_MIXER_BASS   )
#define SOUND_MIXER_WRITE_TREBLE	(SOUND_BASE | SOUND_MIXER | SOUND_WRITE | SOUND_MIXER_TREBLE )
#define SOUND_MIXER_WRITE_MIDI		(SOUND_BASE | SOUND_MIXER | SOUND_WRITE | SOUND_MIXER_MIDI   )
#define SOUND_MIXER_WRITE_PCM		(SOUND_BASE | SOUND_MIXER | SOUND_WRITE | SOUND_MIXER_PCM    )
#define SOUND_MIXER_WRITE_SPEAKER	(SOUND_BASE | SOUND_MIXER | SOUND_WRITE | SOUND_MIXER_SPEAKER)
#define SOUND_MIXER_WRITE_LINE		(SOUND_BASE | SOUND_MIXER | SOUND_WRITE | SOUND_MIXER_LINE   )
#define SOUND_MIXER_WRITE_MIC		(SOUND_BASE | SOUND_MIXER | SOUND_WRITE | SOUND_MIXER_MIC    )

/*
	PCM control - device type 1

	All parameters are of type "unsigned short int".

	RATE = sampling rate in Hz
	CHANNELS = number of channels (1 = mono, 2 = stereo)
	BITS = number of bits/sample/channel (8 = 8 bit, 
		12 = 12 bit, 16 = 16 bit)
	FILTER = flag (0 = don't filter, 1 = use best filter)
*/

#define SOUND_PCM 		0x10
#define SOUND_PCM_TYPE		unsigned short int

#define SOUND_RATE		0x0
#define SOUND_CHANNELS		0x1
#define SOUND_BITS		0x2
#define SOUND_FILTER		0x3

#define SOUND_PCM_READ_RATE		(SOUND_BASE | SOUND_PCM | SOUND_READ  | SOUND_RATE    )
#define SOUND_PCM_READ_CHANNELS		(SOUND_BASE | SOUND_PCM | SOUND_READ  | SOUND_CHANNELS)
#define SOUND_PCM_READ_BITS		(SOUND_BASE | SOUND_PCM | SOUND_READ  | SOUND_BITS    )
#define SOUND_PCM_READ_FILTER		(SOUND_BASE | SOUND_PCM | SOUND_READ  | SOUND_FILTER  )
	
#define SOUND_PCM_WRITE_RATE		(SOUND_BASE | SOUND_PCM | SOUND_WRITE | SOUND_RATE    )
#define SOUND_PCM_WRITE_CHANNELS	(SOUND_BASE | SOUND_PCM | SOUND_WRITE | SOUND_CHANNELS)
#define SOUND_PCM_WRITE_BITS		(SOUND_BASE | SOUND_PCM | SOUND_WRITE | SOUND_BITS    )
#define SOUND_PCM_WRITE_FILTER		(SOUND_BASE | SOUND_PCM | SOUND_WRITE | SOUND_FILTER  )

/*
 *	The Mixer ioctl calls are compatible with mach386 driver by
 *	  Steve Haehnichen <shaehnic@ucsd.edu>
 */

typedef unsigned char BYTE;
typedef unsigned char FLAG;
struct stereo_vol
{
  BYTE l;			/* Left volume */
  BYTE r;			/* Right volume */
};
#define MIXER_IOCTL_SET_LEVELS 		0x02014252
#define MIXER_IOCTL_SET_PARAMS 		0x02024252
#define MIXER_IOCTL_READ_LEVELS		0x02034252
#define MIXER_IOCTL_READ_PARAMS		0x02044252
#define MIXER_IOCTL_RESET		0x02054252

/*
 * Mixer volume levels for MIXER_IOCTL_SET_VOL & MIXER_IOCTL_READ_VOL
 */
struct sb_mixer_levels
{
  struct stereo_vol master;	/* Master volume */
  struct stereo_vol voc;	/* DSP Voice volume */
  struct stereo_vol fm;		/* FM volume */
  struct stereo_vol line;	/* Line-in volume */
  struct stereo_vol cd;		/* CD audio */
  BYTE mic;			/* Microphone level */
};

/*
 * Mixer parameters for MIXER_IOCTL_SET_PARAMS & MIXER_IOCTL_READ_PARAMS
 */
struct sb_mixer_params
{
  BYTE record_source;		/* Recording source (See SRC_xxx below) */
  FLAG hifreq_filter;		/* Filter frequency (hi/low) */
  FLAG filter_input;		/* ANFI input filter */
  FLAG filter_output;		/* DNFI output filter */
  FLAG dsp_stereo;		/* 1 if DSP is in Stereo mode */
};

#define SRC_MIC         1	/* Select Microphone recording source */
#define SRC_CD          3	/* Select CD recording source */
#define SRC_LINE        7	/* Use Line-in for recording source */


/*
 *	Dynamic configuration mechanism.
 *	(for soundload program)
 */


struct soundcard_config
{
	int	config_command;
#define SNDCONF_RESET			0
#define SNDCONF_START			1
#define SNDCONF_SETCARD			2

	int	cardtype;	/* SNDCARD_ADLIB etc. */
	int	card_subtype;	/* Card dependent number */

	int	config_parms[100];	/* Card dependent parameters */
};

extern long soundcard_init(long mem_start);

#endif
