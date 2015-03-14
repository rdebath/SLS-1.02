#include <stdio.h>
#include <malloc.h>
#include <unistd.h>
#include <stdlib.h>
#include <getopt.h>
#include <fcntl.h>
#include <strings.h>
#include <linux/soundcard.h>

#define DEFAULT_DSP_SPEED 8000

#define RECORD	0
#define PLAY	1
#define AUDIO "/dev/dsp"

int timelimit = 0, dsp_speed = DEFAULT_DSP_SPEED, dsp_stereo = 0;
int samplesize = 8;
int quiet_mode = 0;
int audio, abuf_size;
int direction, omode;
char *audiobuf, c;

void recplay (char *name);

int
main (int argc, char *argv[])
{

  char *command;

  command = argv[0];
  if (strstr (argv[0], "srec"))
    {
      direction = RECORD;
      omode = O_RDONLY;
    }
  else if (strstr (argv[0], "splay"))
    {
      direction = PLAY;
      omode = O_WRONLY;
    }
  else
    {
      fprintf (stderr,
	       "Error: command should be named either srec or splay\n");
      exit (1);
    }

  while ((c = getopt (argc, argv, "qs:St:b:")) != EOF)
    switch (c)
      {
      case 'S':
	dsp_stereo = 1;
	break;
      case 'q':
	quiet_mode = 1;
	break;
      case 's':
	dsp_speed = atoi (optarg);
	if (dsp_speed < 300)
	  dsp_speed *= 1000;
	break;
      case 't':
	timelimit = atoi (optarg);
	break;
      case 'b':
	samplesize = atoi (optarg);
	break;
      default:
	fprintf (stderr, 
	 "Usage: %s [-qS] [-t secs] [-s Hz] [-b 8|12|16] [filename]\n", command);
	exit (-1);
      }


  audio = open (AUDIO, omode, 0);
  if (audio == -1)
    {
      perror (AUDIO);
      exit (-1);
    }

  abuf_size = ioctl (audio, SNDCTL_DSP_GETBLKSIZE);
  if (abuf_size < 4096 || abuf_size > 65536)
    {
      if (abuf_size == -1)
	perror (AUDIO);
      else
	fprintf (stderr, "Invalid audio buffers size %d\n", abuf_size);
      exit (-1);
    }

  if ((audiobuf = malloc (abuf_size)) == NULL)
    {
      fprintf (stderr, "Unable to allocate input/output buffer\n");
      exit (-1);
    }

  if (ioctl(audio, SNDCTL_DSP_SAMPLESIZE, samplesize) != samplesize)
  {
  	fprintf(stderr, "Unable to set the sample size\n");
  	exit(-1);
  }

  dsp_stereo = ioctl (audio, SNDCTL_DSP_STEREO, dsp_stereo);

  if ((dsp_speed = ioctl (audio, SNDCTL_DSP_SPEED, dsp_speed)) == -1)
    {
      fprintf (stderr, "%s: Unable to set audio speed\n", command);
      perror (AUDIO);
      exit (-1);
    }
  if (!quiet_mode)
    {
      fprintf (stderr, "Speed %d Hz ", dsp_speed);
      if (dsp_stereo)
	fprintf (stderr, "(stereo)\n");
      else
	fprintf (stderr, "(mono)\n");
      if (samplesize != 8)
        fprintf(stderr, "%d bits per sample\n", samplesize);
    }

  if (optind > argc - 1)
    recplay (NULL);
  else
    while (optind <= argc - 1)
      {
	recplay (argv[optind++]);
      }

  close (audio);
  return 0;
}

void
recplay (char *name)
{
  int fd, l;

  int count, c;

  if (!timelimit)
    count = 0x7fffffff;
  else
    {
      count = timelimit * dsp_speed;
      if (dsp_stereo)
	count *= 2;
      if (samplesize != 8)
        count *= 2;
    }

  if (direction == PLAY)
    {
      if (!name)
	{
	  fd = 0;
	  name = "stdin";
	}
      else
	{
	  if ((fd = open (name, O_RDONLY, 0)) == -1)
	    {
	      perror (name);
	      exit (-1);
	    }
	}

      while (count)
	{
	  c = count;

	  if (c > abuf_size)
	    c = abuf_size;

	  if ((l = read (fd, audiobuf, c))
	      > 0)
	    {
	      if (write (audio, audiobuf, l) != l)
		{
		  perror (AUDIO);
		  exit (-1);
		}
	      count -= l;
	    }
	  else
	    {
	      if (l == -1)
		{
		  perror (name);
		  exit (-1);
		}
	      count = 0;	/* Stop */
	    }

	}			/* while (count) */
      if (fd != 0)
	close (fd);
    }
  else
    {
      if (!name)
	{
	  fd = 1;
	  name = "stdout";
	}
      else
	{
	  if ((fd = open (name, O_WRONLY | O_CREAT, 0666)) == -1)
	    {
	      perror (name);
	      exit (-1);
	    }
	}

      while (count)
	{
	  c = count;
	  if (c > abuf_size)
	    c = abuf_size;

	  if ((l = read (audio, audiobuf, c)) > 0)
	    {
	      if (write (fd, audiobuf, l) != l)
		{
		  perror (name);
		  exit (-1);
		}
	      count -= l;
	    }

	  if (l == -1)
	    {
	      perror (AUDIO);
	      exit (-1);
	    }
	}			/* While count */

      if (fd != 1)
	close (fd);
    }
}
