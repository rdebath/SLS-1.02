/************************************************************************
*									*
*   Copyright 1990, Brown University, Providence, RI.			*
*   Permission to use, copy, modify and distribute this software and	*
*   its documentation for any purpose other than its incorporation	*
*   into a commercial product, is hereby granted, provided that this	*
*   copyright notice appears on all copies.				*
*									*
************************************************************************/
/************************************************************************
*									*
*	speak.c								*
*									*
************************************************************************/
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <multimedia/libaudio.h>
#include <dirent.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>

/************************************************************************
*									*
*	type and array of phoneme audio samples				*
*									*
************************************************************************/
typedef struct phoneme_t {
	int size;
	char *data;
};

static struct phoneme_t phoneme[1024];

/************************************************************************
*									*
*	names of known phonemes						*
*									*
************************************************************************/
static
char *phoneme_name[] = {
	"IY",	"EY",	"AE",	"AO",	"UH",
	"ER",	"AH",	"AW",	"IH",	"EH",
	"AA",	"OW",	"UW",	"AX",	"AY",
	"OY",	"YU",	"p",	"t",	"k",	"f",
	"TH",	"s",	"SH",	"HH",	"n",
	"l",	"y",	"CH",	"WH",	"b",
	"d",	"g",	"v",	"DH",	"z",
	"ZH",	"m",	"NG",	"w",	"r",	"j",
	(char *)0
};

extern FILE *In_file;		/* from parse.c */
extern char *sys_errlist[];

static char *buffer;
static int curfd;

speak_load_samples(dir)
   char *dir;
{
   register i, k, rv;
   char *cp;
   int fd;
   int first = 1;
   char cwd[1024];
   DIR *dirp;
   struct dirent *dp;
   struct stat st;
   Audio_hdr ah, sah;

   if ((dirp = opendir(dir)) == 0)
      return fatal(0, "can't open directory %s", dir);

   (void) getwd(cwd);
   if (chdir(dir))
      return fatal(AUDIO_UNIXERROR, dir);

   while (dp = readdir(dirp)) {
      for (	cp=phoneme_name[i=0];
		cp && strcmp(dp->d_name, cp)!=0;
		cp=phoneme_name[++i]);
      if (cp == (char *)0)
         continue;

      if ((fd = open(dp->d_name, O_RDONLY)) < 0)
         return fatal(AUDIO_UNIXERROR, dp->d_name);
      if ((rv = audio_read_filehdr(fd, &ah, 0, 0)) != AUDIO_SUCCESS)
         return fatal(rv, dp->d_name);
      if (first) {
         bcopy(&ah, &sah);
         first = 0;
      }
/*
      else if (audio_cmp_hdr(&ah, &sah))
         return fatal(0, "phoneme audio headers don't match");
*/

      i = ch_to_code(&cp);
      if (phoneme[i].data)
         free(phoneme[i].data);

      if (ah.data_size == AUDIO_UNKNOWN_SIZE) {		/* pffft */
         if (fstat(fd, &st))
            return fatal(AUDIO_UNIXERROR, dp->d_name);
         phoneme[i].size = st.st_size;
      }
      else
         phoneme[i].size = ah.data_size;

      if ((phoneme[i].data = (char *)malloc(phoneme[i].size)) == 0)
         return fatal(0, "malloc returned zero");

      if ((rv = read(fd, phoneme[i].data, phoneme[i].size)) <= 0)
         return fatal(AUDIO_UNIXERROR, dp->d_name);

      if (rv < phoneme[i].size)
         phoneme[i].size = rv;

      close(fd);
   }
   closedir(dirp);

   for (cp=phoneme_name[i=0]; cp; cp=phoneme_name[++i]) {
      k = ch_to_code(&cp);
      if (phoneme[k].size == 0)
         return fatal(0, "zero length phoneme '%s'", phoneme_name[i]);
   }

   if (chdir(cwd))
      return fatal(AUDIO_UNIXERROR, cwd);

   return 0;
}

int
speak_open(device, now)
   char *device;
   int now;
{
   int fd;
   struct stat st;

   if (device == 0)
      device = "/dev/audio";

   if (stat(device, &st))
      return fatal(AUDIO_UNIXERROR, device);

   if (!S_ISCHR(st.st_mode))
      return fatal(0, "%s is not an audio device", device);

   if ((fd = open(device, O_WRONLY | (now? O_NDELAY:0) )) < 0)
      if (errno == EBUSY)
         return 1;
      else
         return fatal(AUDIO_UNIXERROR, device);

   return fd;
}

speak_string(fd, str)
   int fd;
   char *str;
{
   buffer = str;
   curfd = fd;
   xlate_file();	/* hook into eng_to_phoneme code */
   buffer = (char *)0;

   return 0;
}

speak_file(fd, filename)
   int fd;
   char *filename;
{
   if ((In_file = fopen(filename, "r")) == 0)
      return fatal(0, "can't open %s", filename);

   xlate_file();
   fclose(In_file);
   In_file = (FILE *)0;

   return 0;
}

speak_volume(fd, vol)
   int fd;
   double vol;
{
#ifdef DOCUMENTED_BUT_NOT_IN_LIBAUDIO__SIGH
   audio_set_play_gain(fd, &vol);
#endif
}

speak_close(fd)
   int fd;
{
   close(fd);

   return 0;
}

hacked_getc(fp)
   FILE *fp;
{
   static ix;

   if (In_file)
      return getc(fp);

   if (buffer[ix] == 0) {
      ix = 0;
      return EOF;
   }
   else
      return buffer[ix++];
}

phoneme_str_to_audio(cp)
   char *cp;
{
   register i;

   while (*cp)
      if (isspace(*cp)) {
         cp++;
         speak_delay(0);
      }
      else {
         i = ch_to_code(&cp);
         write(curfd, phoneme[i].data, phoneme[i].size);
      }
}

/************************************************************************
*									*
*	ch_to_code							*
*	code_to_ch							*
*									*
*	Map a phoneme name to a unique index between 1 and 1023, or	*
*	vice versa.  Ch_to_code advances it's argument to the next	*
*	phoneme (or null).  Code_to_ch returns a null-terminated	*
*	string.								*
*									*
************************************************************************/
int
ch_to_code(cp)
   char **cp;
{
   register char *tp = *cp;

   if (islower(*tp)) {
      *cp += 1;
      return *tp - 'a' + 1;
   }
   else {
      *cp += 2;
      return ((*tp - 'A' + 1) << 5) | (*(tp+1) - 'A' + 1);
   }
}

char *
code_to_ch(code)
   int code;
{
   static char ch[3];

   if (code < (1<<5)) {
      ch[1] = code + 'a' - 1;
      return &ch[1];
   }
   else {
      ch[0] = (code >> 5) + 'A' - 1;
      ch[1] = (code & 0x1f) + 'A' - 1;
      return &ch[0];
   }
}

/************************************************************************
*									*
*	speak_delay							*
*									*
*	Flush the audio device and pause for the given interval.	*
*	If no interval is specified, pause for 2/10 second.		*
*									*
************************************************************************/
speak_delay(delay)
   int delay;		/* in 10ths of a second */
{
   static struct timeval tv = {0, 200000};	/* 1sec == 1000000 usec */

   if (delay)
      tv.tv_usec = delay * 100000;
   (void) audio_drain(curfd, FALSE);
   (void) select(0, 0, 0, 0, &tv);

   return 0;
}

int
fatal(code, str, a1, a2, a3, a4, a5, a6, a7, a8, a9)
   int code;
   char *str;
   int a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
   char *bp;
   char b1[256], b2[256];

   bp = (char *)sprintf(b1, str, a1, a2, a3, a4, a5, a6, a7, a8, a9);
   if (code)
      switch (code) {
         case AUDIO_UNIXERROR:
            bp = (char *)sprintf(b2, "%s: %s", str, sys_errlist[errno]);
            break;
         case AUDIO_ERR_BADHDR:
            bp = (char *)sprintf(b2, "%s: bad audio header", str);
            break;
         case AUDIO_ERR_BADFILEHDR:
            bp = (char *)sprintf(b2, "%s: bad file header", str);
            break;
         case AUDIO_ERR_BADARG:
            bp = (char *)sprintf(b2, "%s: bad subroutine argument", str);
            break;
         case AUDIO_ERR_NOEFFECT:
            bp = (char *)sprintf(b2, "%s: device control ingnored", str);
            break;
         case AUDIO_ERR_ENCODING:
            bp = (char *)sprintf(b2, "%s: unknown encoding format", str);
            break;
         case AUDIO_ERR_INTERRUPTED:
            bp = (char *)sprintf(b2, "%s: operation was interrupted", str);
            break;
         default:
            bp = (char *)sprintf(b2, "%s: <unknown error code>", str);
            break;
      }

   fprintf(stderr, "%s\n", bp);

   return -1;
}
