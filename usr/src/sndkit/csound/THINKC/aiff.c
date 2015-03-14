/*
 * Csound routines for writing AIFF files.
 *
 * Bill Gardner, March, 1991.
 */
#include	<unix.h>
#include	"cs.h"
#include	<sfheader.h>
#include	"soundio.h"
#include	"aiff.h"

#define DEBUG	0

static int samp_size;

/*
 * Write with error checking.
 */
aiffWrite(fn,buf,n)
int fn;			/* file descriptor number */
char *buf;		/* ptr to buffer */
int n;			/* number bytes */
{
	if (write(fn,buf,n) != n)
		die("couldn't write the outfile header");
}

/*
 * Seek from beginning with error checking.
 */
aiffSeek(fn,pos)
int fn;			/* file descriptor number */
long pos;		/* from start of file */
{
	if (lseek(fn,pos,0) != pos)
		die("seek error while updating header");
}

/*
 * Write AIFF header information at start of file.
 * Called after opening file, before data writes occur.
 */
aiffWriteHdr(fn,sampsize,nchls,sr)
int fn;			/* file descriptor number */
int sampsize;	/* sample size in bytes */
int nchls;		/* number of channels */
double sr;		/* sampling rate, 96 bits if 68881 (else 80 bits) */
{
AIFFHdr	aiff;

#if DEBUG
	printf("aiffWriteHdr: fn %d sampsize %d nchls %d sr %lf\n",
		fn,sampsize,nchls,sr);
#endif

	samp_size = sampsize;
	aiff.form.ckHdr.ckID = FORM_ID;
	aiff.form.ckHdr.ckSize = 0;		/* leave for aiffReWriteHdr */
	aiff.form.formType = FORM_TYPE;
	aiff.comm.ckHdr.ckID = COMM_ID;
	aiff.comm.ckHdr.ckSize = sizeof(CommonChunk) - sizeof(CkHdr);
	aiff.comm.numChannels = nchls;
	aiff.comm.numSampleFrames = 0;	/* leave for aiffReWriteHdr */
	aiff.comm.sampleSize = sampsize * 8;
#ifdef _MC68881_
	x96tox80(&sr,&aiff.comm.sampleRate);
#else
	aiff.comm.sampleRate = sr;
#endif
	aiff.ssnd.ckHdr.ckID = SSND_ID;
	aiff.ssnd.ckHdr.ckSize = 0;		/* leave for aiffReWriteHdr */
	aiff.ssnd.offset = 0;
	aiff.ssnd.blockSize = 0;
	aiffWrite(fn,&aiff,sizeof(AIFFHdr));
}

/*
 * Write proper sizes into AIFF header. Called before closing file.
 */
aiffReWriteHdr(fn)
int fn;			/* file descriptor number */
{
long endpos;
long numsamps;
long ssnd_size;
long form_size;
#if DEBUG
	printf("aiffReWriteHdr: fn %d\n", fn);
#endif

	endpos = tell(fn);
	numsamps = (endpos - sizeof(AIFFHdr)) / samp_size;
	ssnd_size = endpos - sizeof(FormHdr) - sizeof(CommonChunk) - sizeof(CkHdr);
	form_size = endpos - sizeof(CkHdr);
#if DEBUG
	printf("endpos %ld numsamps %ld ssnd_size %ld form_size %ld\n",
		endpos, numsamps, ssnd_size, form_size);
#endif
	aiffSeek(fn,(long) OFFSET(AIFFHdr,form.ckHdr.ckSize));
	aiffWrite(fn,&form_size,sizeof(long));
	aiffSeek(fn,(long) OFFSET(AIFFHdr,comm.numSampleFrames));
	aiffWrite(fn,&numsamps,sizeof(long));
	aiffSeek(fn,(long) OFFSET(AIFFHdr,ssnd.ckHdr.ckSize));
	aiffWrite(fn,&ssnd_size,sizeof(long));
}

/*
 * Read AIFF header data, position read ptr of file to start of samples,
 */
HEADATA *aiffReadHeader(fn,fname,hdr)
int fn;			/* file descriptor number */
char *fname;
HEADATA *hdr;	/* datablock to return */
{
FormHdr form;
CkHdr ckHdr;
CommonChunk comm;
SoundDataHdr ssnd;
int comm_read = 0;
int ssnd_read = 0;
long ssnd_pos;
long pos;
double sr;
	/* read the first long and check it */
	readin(fn,&form,sizeof(long));
	if (form.ckHdr.ckID == FORM_ID) {
		/* read the rest of FormHdr */
		readin(fn,(char *)&form + sizeof(long),sizeof(FormHdr) - sizeof(long));
		if (form.formType != FORM_TYPE)
			die("bad form type in header");
		hdr->readlong = FALSE;
		hdr->firstlong = 0;
	}
	else {
		/* no header, assume raw data */
		hdr->hdrsize = 0;
		hdr->readlong = TRUE;
		hdr->firstlong = form.ckHdr.ckID;
		return hdr;
	}
	while (1) {
		readin(fn,&ckHdr,sizeof(CkHdr));
		pos = tell(fn);
		if (ckHdr.ckID == COMM_ID) {
			/* read remainder of CommonChunk */
			readin(fn,(char *)&comm + sizeof(CkHdr),
				sizeof(CommonChunk) - sizeof(CkHdr));
			/* parse CommonChunk to hdr format */
			if (comm.sampleSize <= 8) {
				hdr->format = AE_CHAR;
				hdr->sampsize = sizeof(char);
			}
			else if (comm.sampleSize <= 16) {
				hdr->format = AE_SHORT;
				hdr->sampsize = sizeof(short);
			}
			else {
				hdr->format = AE_LONG;
				hdr->sampsize = sizeof(long);
			}
			hdr->nchnls = comm.numChannels;
#ifdef _MC68881_
			x80tox96(&comm.sampleRate,&sr);
			hdr->sr = (long) sr;
#else
			hdr->sr = (long) comm.sampleRate;
#endif
			/* set CommonChunk read flag */
			comm_read = TRUE;
		}
		else if (ckHdr.ckID == SSND_ID) {
			/* read remainder of SoundDataHdr */
			readin(fn,(char *)&ssnd + sizeof(CkHdr),
				sizeof(SoundDataHdr) - sizeof(CkHdr));
			ssnd_pos = pos - sizeof(CkHdr) + sizeof(SoundDataHdr) + ssnd.offset;
			hdr->hdrsize = ssnd_pos;
			ssnd_read = TRUE;
		}
		/* if both CommonChunk and SoundDataHdr read, then we're done */
		if (comm_read && ssnd_read) {
			if (lseek(fn,ssnd_pos,0) != ssnd_pos)
				die("error seeking to start of sound data");
			else return hdr;
		}
		/* seek past this chunk to next one */
		if (lseek(fn,pos + ckHdr.ckSize,0) != pos + ckHdr.ckSize)
			die("error while seeking past AIFF chunk");
	}
}
