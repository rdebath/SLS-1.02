#include <stdio.h>

#define USAGE		"usage: scat [-v vol] [-d directory]\n"
#define DEFAULT_DIR	"/cs/data/phonemes"
#define DEFAULT_VOL	.5

static void usage();

main (argc, argv)
   int argc;
   char **argv;
{
   int fd;
   double vol = DEFAULT_VOL;
   char *dir = (char *)0;
   char cp[1024];

   while (--argc)
      if (**++argv == '-')
         switch (*(*argv+1)) {
            case 'v':
               if (--argc)
                  vol = (double)atof(*++argv);
               else
                  usage();
               break;
            case 'd':
               if (--argc)
                  dir = *++argv;
               else
                  usage();
               break;
            default:
               usage();
         }
      else
         usage();

   if (dir == (char *)0)
      dir = DEFAULT_DIR;

   if (speak_load_samples(dir))
      exit(1);

   fd = speak_open(0, 0);

   if (vol < 0.0)
      vol = 0.0;
   else if (vol > 1.0)
      vol = 1.0;

   speak_volume(fd, vol);

   while (gets(cp))
      speak_string(fd, cp);

   speak_close(fd);
}

static void
usage()
{
   fprintf(stderr, USAGE);
   exit(1);
}
