			/*						SOUNDIO.H	*/

#ifdef SFSUN41
#include <multimedia/audio_hdr.h>
#include <multimedia/libaudio.h>
#endif

#define	IOBUFSAMPS   4096	/* default samples in audio iobuffer, settable by user */
#define	SNDINBUFSIZ  4096	/* soundin bufsize;   must be > sizeof(SFHEADER), */
                                /*                 but small is kind to net rexec */
#define	SOUNDIN_NAME "soundin."

# define AE_CHAR   0x101          /* audio encoding types */
# define AE_ALAW   0x102
# define AE_ULAW   0x103
# define AE_SHORT  0x104
# define AE_LONG   0x105
# define AE_FLOAT  0x106

# define SF_MAXCHAN	4
# define SF_MINCOM      400

# define SF_END         0         /* SFCODE types */
# define SF_MAXAMP      1
# define SF_AUDIOENCOD  2
# define SF_PVDATA      3
# define SF_COMMENT     4
# define SF_CODMAX      4

typedef struct {                  /* this code written by soundio */
	float	value[SF_MAXCHAN];
} SFMAXAMP;

typedef struct {                  /*     ditto                    */
        short   encoding;
	short   grouping;
} SFAUDIOENCOD;

typedef struct {                  /* this code written by pvanal */
        short   frameSize;
	short   frameIncr;
} SFPVDATA;

typedef struct {                  /* this code not possible yet */
        char    comment[SF_MINCOM];
} SFCOMMENT;

typedef struct {                  /* for passing data to/from sfheader routines */
  long  sr;
  long  nchnls;
  long  sampsize;
  long  format;
  long  hdrsize;
  long  readlong;
  long  firstlong;
} HEADATA;

typedef	struct {
	OPDS	h;
	float	*r1, *r2, *r3, *r4, *ifilno, *iskptim, *iformat;
	FDCH	fdch;
	int	format, sampframsiz, endfile;
	char	*inbufp, *bufend;
	char	inbuf[SNDINBUFSIZ];
} SOUNDIN;
