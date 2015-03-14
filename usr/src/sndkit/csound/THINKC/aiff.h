/*
 * Definitions for writing AIFF files.
 *
 * Bill Gardner, March 1991.
 */

#include	<SANE.h>

typedef long ID;

typedef struct {
	ID		ckID;
	long	ckSize;
} CkHdr;

typedef struct {
	CkHdr	ckHdr;
	ID		formType;
} FormHdr;

typedef struct {
	CkHdr		ckHdr;
	short		numChannels;
	long		numSampleFrames;
	short		sampleSize;
	extended	sampleRate;
} CommonChunk;

typedef struct {
	CkHdr	ckHdr;
	long	offset;
	long	blockSize;
} SoundDataHdr;

/*
 * Form, CommonChunk, and SoundData lumped into one.
 */
typedef struct {
	FormHdr			form;
	CommonChunk		comm;
	SoundDataHdr	ssnd;
} AIFFHdr;

#define	FORM_ID		((ID) 'FORM')
#define	COMM_ID		((ID) 'COMM')
#define	FORM_TYPE	((ID) 'AIFF')
#define SSND_ID		((ID) 'SSND')
