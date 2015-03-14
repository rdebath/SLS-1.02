/* song.h */

/* internal data structures for the soundtracker player routine....
 */

/* $Id: song.h,v 3.2 1992/11/23 10:12:23 espie Exp espie $
 * $Log: song.h,v $
 * Revision 3.2  1992/11/23  10:12:23  espie
 * *** empty log message ***
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 2.5  1992/10/31  11:18:00  espie
 * New fields for optimized resampling.
 * Exchanged __ANSI__ to SIGNED #define.
 */

#ifdef SIGNED
typedef signed char SAMPLE;
#else
typedef char SAMPLE;
#endif

#define NUMBER_SAMPLES 32

#define BLOCK_LENGTH 64
#define NUMBER_TRACKS 4
#define NUMBER_PATTERNS 128

#define NUMBER_EFFECTS 16

#define SAMPLENAME_MAXLENGTH 22
#define TITLE_MAXLENGTH 20

#define MIN_PITCH 113
#define MAX_PITCH 856

#define MIN_VOLUME 0
#define MAX_VOLUME 64

/* the fuzz in note pitch */
#define FUZZ 2

/* we refuse to allocate more than 500000 bytes for one sample */
#define MAX_SAMPLE_LENGTH 500000

struct sample_info
   {
   char *name;
   int  length, rp_offset, rp_length;
   unsigned long  fix_length, fix_rp_length;
   int volume;
   int finetune;
   SAMPLE *start, *rp_start;
   };

/* the actual parameters may be split in two halves occasionnally */

#define LOW(para) ((para) & 15)
#define HI(para) ((para) >> 4)

struct event
   {
   unsigned char sample_number;
   unsigned char effect;
   unsigned char parameters;
   unsigned char note;
   int pitch;
   };

struct block
   {
   struct event e[NUMBER_TRACKS][BLOCK_LENGTH];
   };
    
        
struct song_info
   {
   int length;
   int maxpat;
   int transpose;
   char patnumber[NUMBER_PATTERNS];
   struct block *pblocks;
   };

struct song
   {
   char *title;
      /* sample 0 is always a dummy sample */
   struct sample_info samples[NUMBER_SAMPLES];
   struct song_info info;
   };

#define AMIGA_CLOCKFREQ 3575872
