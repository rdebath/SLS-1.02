/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  zip.c by Mark Adler.
 */

#include "revision.h"
#include "zip.h"
#include <signal.h>

#define PWLEN 80        /* Input buffer size for reading encryption key */
#define MAXCOM 256      /* Maximum one-line comment size */


/* Local option flags */
#define DELETE  0
#define ADD     1
#define UPDATE  2
#define FRESHEN 3
local int action = ADD; /* one of ADD, UPDATE, FRESHEN, or DELETE */
local int comadd = 0;   /* 1=add comments for new files */
local int zipedit = 0;  /* 1=edit zip comment and all file comments */
local int dispose = 0;  /* 1=remove files after put in zip file */
local int latest = 0;   /* 1=set zip file time to time of latest file */
local ulg before = 0;   /* 0=ignore, else exclude files before this time */


/* Temporary zip file name and file pointer */
local char *tempzip;
local FILE *tempzf;


/* Local functions */
#ifdef PROTO
   local void freeup(void);
   local void leave(int);
   local void err(int, char *);
   local void handler(int);
   local void license(void);
   local void help(void);
   void main(int, char **);
#endif /* PROTO */



local void freeup()
/* Free all allocations in the found list and the zfiles list */
{
  struct flist far *f;  /* steps through found list */
  struct zlist far *z;  /* pointer to next entry in zfiles list */

  for (f = found; f != NULL; f = fexpel(f))
    ;
  while (zfiles != NULL)
  {
    z = zfiles->nxt;
    free((voidp *)(zfiles->name));
    free((voidp *)(zfiles->zname));
    if (zfiles->ext)
      free((voidp *)(zfiles->extra));
    if (zfiles->cext && zfiles->cextra != zfiles->extra)
      free((voidp *)(zfiles->cextra));
    if (zfiles->com)
      free((voidp *)(zfiles->comment));
    farfree((voidp far *)zfiles);
    zfiles = z;
    zcount--;
  }
}


local void leave(e)
int e;                  /* exit code */
/* Process -o and -m options (if specified), free up malloc'ed stuff, and
   exit with the code e. */
{
  int r;                /* return value from trash() */
  ulg t;                /* latest time in zip file */
  struct zlist far *z;  /* pointer into zfile list */

  /* If latest, set time to zip file to latest file in zip file */
  if (latest)
  {
    diag("changing time of zip file to time of latest file in it");
    /* find latest time in zip file */
    t = zfiles->tim;
    for (z = zfiles->nxt; z != NULL; z = z->nxt)
      if (t < z->tim)
        t = z->tim;
    /* set modified time of zip file to that time */
    stamp(zipfile, t);
  }
  if (tempath != NULL)
  {
    free((voidp *)tempath);
    tempath = NULL;
  }
  if (zipfile != NULL)
  {
    free((voidp *)zipfile);
    zipfile = NULL;
  }


  /* If dispose, delete all files in the zfiles list that are marked */
  if (dispose)
  {
    diag("deleting files that were added to zip file");
    if ((r = trash()) != ZE_OK)
      err(r, "was deleting moved files and directories");
  }


  /* Done! */
  freeup();
#ifdef VMS
  exit(0);
#else /* !VMS */
  exit(e);
#endif /* ?VMS */
}


local void err(c, h)
int c;                  /* error code from the ZE_ class */
char *h;                /* message about how it happened */
/* Issue a message for the error, clean up files and memory, and exit. */
{
  if (PERR(c))
    perror("zip error");
  fprintf(stderr, "zip error: %s (%s)\n", errors[c-1], h);
  if (shract)
  {
    shr_clear();
    shract = 0;
  }
#ifndef NOIMPLODE
  if (impact)
  {
    imp_clear();
    impact = 0;
  }
#endif /* !NOIMPLODE */
  if (tempzf != NULL)
    fclose(tempzf);
  if (tempzip != NULL)
  {
    destroy(tempzip);
    if (tempzip != zipfile)
      free((voidp *)tempzip);
  }
  if (key != NULL)
    free((voidp *)key);
  if (tempath != NULL)
    free((voidp *)tempath);
  if (zipfile != NULL)
    free((voidp *)zipfile);
  freeup();
#ifdef VMS
  exit(0);
#else /* !VMS */
  exit(c);
#endif /* ?VMS */
}


local void handler(s)
int s;                  /* signal number (ignored) */
/* Upon getting a user interrupt, turn echo back on for tty and abort
   cleanly using err(). */
{
#ifndef MSVMS
#ifndef EXPORT
  echon();
#endif /* !EXPORT */
  putc('\n', stderr);
#endif /* !MSVMS */
  err(ZE_ABORT, "aborting");
  s++;                                  /* keep some compilers happy */
}


void warn(a, b)
char *a, *b;            /* message strings juxtaposed in output */
/* Print a warning message to stderr and return. */
{
  fprintf(stderr, "zip warning: %s%s\n", a, b);
}


local void license()
/* Print license information to stdout. */
{
  extent i;             /* counter for copyright array */

  for (i = 0; i < sizeof(copyright)/sizeof(char *); i++)
    puts(copyright[i]);
  for (i = 0; i < sizeof(disclaimer)/sizeof(char *); i++)
    puts(disclaimer[i]);
}


local void help()
/* Print help (along with license info) to stdout. */
{
  extent i;             /* counter for help array */

  /* help array */
  static char *text[] = {
"",
"Zip %d.%d (%s)",
"Usage:  zip [-options] [-b path] [-t mmddyy] zipfile list [-x list]",
"  the default action is to add or replace zipfile entries from list, which",
"  can include the special name - to read names from stdin.",
"  -f   freshen: only changed files  -u   update: only changed or new files",
"  -d   delete entries in zipfile    -m   move into zipfile (delete files)",
"  -k   simulate PKZIP made zipfile  -g   allow growing existing zipfile",
"  -h   show this help               -l   show software license",
"  -r   recurse into directories     -j   junk (don't record) directory names",
"  -i   implode only                 -s   shrink only",
"  -0   compress faster              -9   compress better",
"  -q   quiet operation              -n   don't compress special suffixes",
"  -c   add one-line comments        -z   add zipfile comment",
"  -b   use \"path\" for temp files    -t   only do files after \"mmddyy\"",
#ifdef EXPORT
"  -o   make zipfile as old as latest entry",
#else /* !EXPORT */
"  -e   encrypt  (-ee verify key)    -o   make zipfile as old as latest entry",
#endif /* ?EXPORT */
#ifdef VMS
"  -w   append the VMS version number to the name stored in the zip file",
#endif /* VMS */
#ifdef S_IFLNK
"  -y   store symbolic links as the link instead of the referenced file",
#endif /* !S_IFLNK */
"  -x   exclude the names that follow from those operated on"
  };

  for (i = 0; i < sizeof(copyright)/sizeof(char *); i++)
    puts(copyright[i]);
  for (i = 0; i < sizeof(text)/sizeof(char *); i++)
  {
    printf(text[i], REVISION / 10, REVISION % 10, REVDATE);
    putchar('\n');
  }
}


/* Do command line expansion for MSDOS and VMS */
#ifdef MSVMS
#  define PROCNAME(n) (action==ADD||action==UPDATE?wild(n):procname(n))
#else /* !MSVMS */
#  define PROCNAME(n) procname(n)
#endif /* ?MSVMS */


void main(argc, argv)
int argc;               /* number of tokens in command line */
char **argv;            /* command line tokens */
/* Add, update, freshen, or delete zip entries in a zip file.  See the
   command help in help() above. */
{
  int a;                /* attributes of zip file */
  ulg c;                /* start of central directory */
  int d;                /* true if just adding to a zip file */
  char *e;              /* malloc'd comment buffer */
  struct flist far *f;  /* steps through found linked list */
  int i;                /* arg counter, root directory flag */
  int k;                /* next argument type, marked counter,
                           comment size, entry count */
  ulg n;                /* total of entry len's */
  int o;                /* true if there were any ZE_OPEN errors */
  char *p;              /* steps through option arguments */
  int r;                /* temporary variable */
  ulg t;                /* file time, length of central directory */
  struct zlist far *v;  /* temporary variable */
  struct zlist far * far *w;    /* pointer to last link in zfiles list */
  FILE *x, *y;          /* input and output zip files */
  struct zlist far *z;  /* steps through zfiles linked list */


  /* Process arguments */
  diag("processing arguments");
  if (argc == 1)
  {
    help();
    exit(0);
  }
  zipfile = tempzip = NULL;
  tempzf = NULL;
  d = 0;                        /* disallow adding to a zip file */
  signal(SIGINT, handler);
  signal(SIGTERM, handler);
  k = 0;                        /* Next non-option argument type */
  for (i = 1; i < argc; i++)
  {
    if (argv[i][0] == '-')
      if (argv[i][1])
        for (p = argv[i]+1; *p; p++)
          switch(*p)
          {
            case '0':  case '1':  case '2':  case '3':  case '4':
            case '5':  case '6':  case '7':  case '8':  case '9':
                        /* Set the compression efficacy */
              level = *p - '0';  break;
            case 'b':   /* Specify path for temporary file */
              if (k != 0)
                err(ZE_PARMS, "use -b before zip file name");
              else
                k = 1;          /* Next non-option is path */
              break;
            case 'c':   /* Add comments for new files in zip file */
              comadd = 1;  break;
            case 'd':   /* Delete files from zip file */
              if (action != ADD)
                err(ZE_PARMS, "specify just one action");
              action = DELETE;
              break;
#ifndef EXPORT
            case 'e':   /* Encrypt */
              e = key == NULL ? (char *)NULL : key;
              if ((key = malloc(PWLEN+1)) == NULL)
                err(ZE_MEM, "was getting encryption password");
              if (getp(e == NULL ? "Enter password: " : "Verify password: ",
                       key, PWLEN+1) == NULL)
                err(ZE_PARMS, "stderr is not a tty");
              if (e != NULL)
              {
                r = strcmp(key, e);
                free((voidp *)e);
                if (r)
                  err(ZE_PARMS, "password not verified");
              }
              break;
#endif /* !EXPORT */
            case 'f':   /* Freshen zip file--overwrite only */
              if (action != ADD)
                err(ZE_PARMS, "specify just one action");
              action = FRESHEN;
              break;
            case 'g':   /* Allow appending to a zip file */
              d = 1;  break;
            case 'h':   /* Help */
              help();  break;
            case 'i':   /* Implode only */
              method = IMPLODE;  break;
            case 'j':   /* Junk directory names */
              pathput = 0;  break;
            case 'k':   /* Make entries using DOS names (k for Katz) */
              dosify = 2;  break;
            case 'l':   /* Show license, version */
              license();  break;
            case 'm':   /* Delete files added or updated in zip file */
              dispose = 1;  break;
            case 'n':   /* Don't compress files with a special suffix */
              if ((special = getenv("NOZIP")) == NULL)
                special = ".Z:.zip:.zoo:.arc";
#ifndef OS2
#ifdef MSDOS
              strupr(special);
#endif /* MSDOS */
#endif /* !OS2 */
              break;
            case 'o':   /* Set zip file time to time of latest file in it */
              latest = 1;  break;
            case 'p':   /* Store path with name */
              break;            /* (do nothing as annoyance avoidance) */
            case 'q':   /* Quiet operation */
              noisy = 0;  break;
            case 'r':   /* Recurse into subdirectories */
              recurse = 1;  break;
            case 's':   /* Shrink only */
              method = SHRINK;  break;
            case 't':   /* Exclude files earlier than specified date */
              if (before)
                err(ZE_PARMS, "can only have one -t");
              k = 2;  break;
            case 'u':   /* Update zip file--overwrite only if newer */
              if (action != ADD)
                err(ZE_PARMS, "specify just one action");
              action = UPDATE;
              break;
            case 'v':   /* Mention oddities in zip file structure */
              verbose = 1;
              break;
#ifdef VMS
            case 'w':   /* Append the VMS version number */
              vmsver = 1;  break;
#endif /* VMS */
            case 'x':   /* Exclude following files */
              if (k != 4 &&
                  (k != 3 || (action != UPDATE && action != FRESHEN)))
                err(ZE_PARMS, "nothing to exclude (-x) from");
              if (k == 3)       /* must be -u or -f */
                for (z = zfiles; z != NULL; z = z->nxt)
                  z->mark = 1;  /* mark all of them */
              k = 5;
              if ((r = exclude()) != ZE_OK)
                if (r == ZE_PARMS)
                  err(r, "cannot repeat names in zip file");
                else
                  err(r, "was processing list of files");
              break;
#ifdef S_IFLNK
            case 'y':   /* Store symbolic links as such */
              linkput = 1;  break;
#endif /* !S_IFLNK */
            case 'z':   /* Edit zip file comment */
              zipedit = 1;  break;
            default:
            {
              sprintf(errbuf, "no such option: %c", *p);
              err(ZE_PARMS, errbuf);
            }
          }
      else              /* just a dash */
        if (k < 3)
          err(ZE_PARMS, "zip file cannot be stdin");
        else            /* read names from stdin */
          while ((p = getnam(errbuf)) != NULL)
          {
            if ((r = PROCNAME(p)) != ZE_OK)
              if (r == ZE_MISS)
                warn("name not matched: ", p);
              else
                err(r, p);
          }
    else                /* not an option */
    {
      switch (k)
      {
        case 0:
          if ((zipfile = ziptyp(argv[i])) == NULL)
            err(ZE_MEM, "was processing arguments");
          if ((r = readzipfile()) != ZE_OK)
            err(r, zipfile);
          found = NULL;
          fnxt = &found;
          k = 3;
          break;
        case 1:
          if ((tempath = malloc(strlen(argv[i]) + 1)) == NULL)
            err(ZE_MEM, "was processing arguments");
          strcpy(tempath, argv[i]);
          k = 0;
          break;
        case 2:
        {
          int yy, mm, dd;       /* results of sscanf() */

          if (sscanf(argv[i], "%2d%2d%2d", &mm, &dd, &yy) != 3 ||
              mm < 1 || mm > 12 || dd < 1 || dd > 31)
            err(ZE_PARMS, "invalid date entered for -t option");
          before = dostime(yy + (yy < 80 ? 2000 : 1900), mm, dd, 0, 0, 0);
          k = 0;
          break;
        }
        case 3:  case 4:  case 5:
          if ((r = PROCNAME(argv[i])) != ZE_OK)
            if (r == ZE_MISS)
              warn("name not matched: ", argv[i]);
            else
              err(r, argv[i]);
          if (k == 3)
            k = 4;
      }
    }
  }
  if (k < 3)
    exit(0);                    /* No zip file, don't complain */
  if (k != 5)                   /* Clean up selections */
  {
    if (k == 3 && (action == UPDATE || action == FRESHEN))
      for (z = zfiles; z != NULL; z = z->nxt)
        z->mark = 1;                    /* if -u or -f with no args, do all */
    if ((r = exclude()) != ZE_OK)       /* remove duplicates in found list */
      if (r == ZE_PARMS)
        err(r, "cannot repeat names in zip file");
      else
        err(r, "was processing list of files");
  }
  if (zcount)
    free((voidp *)zsort);


  /* Check option combinations */
  if (action == DELETE && (method != BEST || dispose || recurse ||
      dosify || key != NULL || comadd || zipedit))
    err(ZE_PARMS, "invalid option(s) used with -d");
  if (linkput && dosify)
    err(ZE_PARMS, "can't use -y with -k");

  /* If -b not specified, make temporary path the same as the zip file */
#ifdef MSDOS
  if (tempath == NULL && ((p = strrchr(zipfile, '/')) != NULL ||
                          (p = strrchr(zipfile, '\\')) != NULL ||
                          (p = strrchr(zipfile, ':')) != NULL))
  {
    if (*p == ':')
      p++;
#else /* !MSDOS */
  if (tempath == NULL && (p = strrchr(zipfile, '/')) != NULL)
  {
#endif /* ?MSDOS */
    if ((tempath = malloc((int)(p - zipfile) + 1)) == NULL)
      err(ZE_MEM, "was processing arguments");
    r = *p;  *p = 0;
    strcpy(tempath, zipfile);
    *p = (char)r;
  }

  /* If under MSDOS, force entries to look like made by PKZIP */
#ifndef OS2
#ifdef MSDOS
  dosify = 1;
#endif /* MSDOS */
#endif /* !OS2 */


  /* For each marked entry, if not deleting, check if it exists, and if
     updating or freshening, compare date with entry in old zip file.
     Unmark if it doesn't exist or is too old, else update marked count. */
  diag("stating marked entries");
  k = 0;                        /* Initialize marked count */
  for (z = zfiles; z != NULL; z = z->nxt)
    if (z->mark)
      if (action != DELETE &&
                ((t = filetime(z->name, (ulg *)NULL, (long *)NULL)) == 0 ||
                 t < before ||
                 ((action == UPDATE || action == FRESHEN) && t <= z->tim)))
      {
        z->mark = 0;
        z->trash = t && t >= before;    /* delete if -um or -fm */
        if (verbose)
          printf("zip diagnostic: %s %s\n", z->name,
                 z->trash ? "up to date" : "missing or early");
      }
      else
        k++;


  /* Remove entries from found list that do not exist or are too old */
  diag("stating new entries");
  for (f = found; f != NULL;)
    if (action == DELETE || action == FRESHEN ||
        (t = filetime(f->name, (ulg *)NULL, (long *)NULL)) == 0 ||
        t < before || strcmp(f->name, zipfile) == 0)
      f = fexpel(f);
    else
      f = f->nxt;


  /* Make sure there's something left to do */
  if (k == 0 && found == NULL && !(zfiles != NULL && (latest || zipedit)))
    if (action == UPDATE || action == FRESHEN)
      leave(ZE_OK);
    else
      err(ZE_NONE, zipfile);
  d = (d && k == 0 && zfiles != NULL);  /* d true if just appending */


  /* Before we get carried away, make sure zip file is writeable */
  if ((x = fopen(zipfile, zfiles == NULL ? FOPW : FOPM)) == NULL)
    err(ZE_CREAT, zipfile);
  fclose(x);
  a = getfileattr(zipfile);
  if (zfiles == NULL)
    destroy(zipfile);


  /* Open zip file and temporary output file */
  diag("opening zip file and creating temporary zip file");
  x = NULL;
  if (d)
  {
    if ((y = fopen(zipfile, FOPM)) == NULL)
      err(ZE_NAME, zipfile);
    tempzip = zipfile;
    tempzf = y;
#ifdef MSDOS
    {
      char *zipbuf;

      zipbuf = (char *)malloc(BSZ);
      if (zipbuf == NULL)
        err(ZE_MEM, tempzip);
      setbuf(y, zipbuf);
    }
#endif /* MSDOS */
    if (fseek(y, cenbeg, SEEK_SET))
      err(ferror(y) ? ZE_READ : ZE_EOF, zipfile);
  }
  else
  {
    if (zfiles != NULL && (x = fopen(zipfile, FOPR)) == NULL)
      err(ZE_NAME, zipfile);
    if ((tempzip = tempname('Z')) == NULL)
      err(ZE_MEM, tempzip);
    if ((tempzf = y = fopen(tempzip, FOPW)) == NULL)
      err(ZE_TEMP, tempzip);
    if (zipbeg && (r = fcopy(x, y, zipbeg)) != ZE_OK)
      err(r, r == ZE_TEMP ? tempzip : zipfile);
  }
  o = 0;                                /* no ZE_OPEN errors yet */


  /* Process zip file, updating marked files */
  if (zfiles != NULL)
    diag("going through old zip file");
  w = &zfiles;
  while ((z = *w) != NULL)
    if (z->mark)
    {
      /* if not deleting, zip it up */
      if (action != DELETE)
      {
        if (noisy)
        {
          printf("updating %s", z->zname);
          fflush(stdout);
        }
        if ((r = zipup(z, y)) != ZE_OK && r != ZE_OPEN)
        {
          if (noisy)
          {
            putchar('\n');
            fflush(stdout);
          }
          sprintf(errbuf, "was zipping %s", z->name);
          err(r, errbuf);
        }
        if (r == ZE_OPEN)
        {
          o = 1;
          if (noisy)
          {
            putchar('\n');
            fflush(stdout);
          }
          perror("zip warning");
          warn("could not open for reading: ", z->name);
          warn("will just copy entry over: ", z->zname);
          if ((r = zipcopy(z, x, y)) != ZE_OK)
          {
            sprintf(errbuf, "was copying %s", z->zname);
            err(r, errbuf);
          }
          z->mark = 0;
        }
        w = &z->nxt;
      }
      else
      {
        if (noisy)
        {
          printf("deleting %s\n", z->zname);
          fflush(stdout);
        }
        v = z->nxt;                     /* delete entry from list */
        free((voidp *)(z->name));
        free((voidp *)(z->zname));
        if (z->ext)
          free((voidp *)(z->extra));
        if (z->cext && z->cextra != z->extra)
          free((voidp *)(z->cextra));
        if (z->com)
          free((voidp *)(z->comment));
        farfree((voidp far *)z);
        *w = v;
        zcount--;
      }
    }
    else
    {
      /* copy the original entry verbatim */
      if (!d && (r = zipcopy(z, x, y)) != ZE_OK)
      {
        sprintf(errbuf, "was copying %s", z->zname);
        err(r, errbuf);
      }
      w = &z->nxt;
    }
  if (x != NULL)
    fclose(x);


  /* Process the edited found list, adding them to the zip file */
  diag("zipping up new entries, if any");
  for (f = found; f != NULL; f = fexpel(f))
  {
    /* add a new zfiles entry and set the name */
    if ((z = (struct zlist far *)farmalloc(sizeof(struct zlist))) == NULL)
      err(ZE_MEM, "was adding files to zip file");
    z->nxt = NULL;
    z->name = f->name;
    f->name = NULL;
    z->zname = f->zname;
    f->zname = NULL;
    z->ext = z->cext = z->com = 0;
    z->mark = 1;
    /* zip it up */
    if (noisy)
    {
      printf("adding %s", z->zname);
      fflush(stdout);
    }
    if ((r = zipup(z, y)) != ZE_OK  && r != ZE_OPEN)
    {
      if (noisy)
      {
        putchar('\n');
        fflush(stdout);
      }
      sprintf(errbuf, "was zipping %s", z->name);
      err(r, errbuf);
    }
    if (r == ZE_OPEN)
    {
      o = 1;
      if (noisy)
      {
        putchar('\n');
        fflush(stdout);
      }
      perror("zip warning");
      warn("could not open for reading: ", z->name);
      free((voidp *)(z->name));
      free((voidp *)(z->zname));
      farfree((voidp far *)z);
    }
    else
    {
      *w = z;
      w = &z->nxt;
      zcount++;
    }
  }
  if (key != NULL)
  {
    free((voidp *)key);
    key = NULL;
  }


  /* Get one line comment for each new entry */
  if (comadd)
  {
    if ((e = malloc(MAXCOM + 1)) == NULL)
      err(ZE_MEM, "was reading comment lines");
    for (z = zfiles; z != NULL; z = z->nxt)
      if (z->mark)
      {
        if (noisy)
          printf("Enter comment for %s:\n", z->name);
        if (fgets(e, MAXCOM+1, stdin) != NULL)
        {
          if ((p = malloc((k = strlen(e))+1)) == NULL)
          {
            free((voidp *)e);
            err(ZE_MEM, "was reading comment lines");
          }
          strcpy(p, e);
          if (p[k-1] == '\n')
            p[--k] = 0;
          z->comment = p;
          z->com = k;
        }
      }
    free((voidp *)e);
  }

  /* Get multi-line comment for the zip file */
  if (zipedit)
  {
    if ((e = malloc(MAXCOM + 1)) == NULL)
      err(ZE_MEM, "was reading comment lines");
    if (noisy && zcomlen)
    {
      puts("current zip file comment is:");
      fwrite(zcomment, 1, zcomlen, stdout);
      if (zcomment[zcomlen-1] != '\n')
        putchar('\n');
      free((voidp *)zcomment);
    }
    zcomment = malloc(1);
    *zcomment = 0;
    if (noisy)
      puts("enter new zip file comment (end with .):");
    while (fgets(e, MAXCOM+1, stdin) != NULL && strcmp(e, ".\n"))
    {
      if (e[(r = strlen(e)) - 1] == '\n')
        e[--r] = 0;
      if ((p = malloc((*zcomment ? strlen(zcomment) + 3 : 1) + r)) == NULL)
      {
        free((voidp *)e);
        err(ZE_MEM, "was reading comment lines");
      }
      if (*zcomment)
        strcat(strcat(strcpy(p, zcomment), "\r\n"), e);
      else
        strcpy(p, *e ? e : "\r\n");
      free((voidp *)zcomment);
      zcomment = p;
    }
    zcomlen = strlen(zcomment);
    free((voidp *)e);
  }


  /* Write central directory and end header to temporary zip */
  diag("writing central directory");
  k = 0;                        /* keep count for end header */
  if ((c = ftell(y)) == -1L)    /* get start of central */
    err(d ? ZE_WRITE : ZE_TEMP, tempzip);
  n = t = 0;
  for (z = zfiles; z != NULL; z = z->nxt)
  {
    if ((r = putcentral(z, y)) != ZE_OK)
      err(r, tempzip);
    n += z->len;
    t += z->siz;
    k++;
  }
  if (k == 0)
    warn("zip file empty", "");
  if (verbose)
    printf("total bytes=%lu, compressed=%lu -> %d%% savings\n",
           n, t, percent(n, t));
  if ((t = ftell(y)) == -1L)    /* get end of central */
    err(d ? ZE_WRITE : ZE_TEMP, tempzip);
  t -= c;                       /* compute length of central */
  diag("writing end of central directory");
  if ((r = putend(k, t, c, zcomlen, zcomment, y)) != ZE_OK)
    err(r, tempzip);
  tempzf = NULL;
  if (fclose(y))
    err(d ? ZE_WRITE : ZE_TEMP, tempzip);


  /* Replace old zip file with new zip file, leaving only the new one */
  if (!d)
  {
    diag("replacing old zip file with new zip file");
    if ((r = replace(zipfile, tempzip)) != ZE_OK)
    {
      warn("new zip file left as: ", tempzip);
      free((voidp *)tempzip);
      tempzip = NULL;
      err(r, "was replacing the original zip file");
    }
    free((voidp *)tempzip);
  }
  tempzip = NULL;
  setfileattr(zipfile, a);

  /* Finish up (process -o, -m, clean up).  Exit code depends on o. */
  leave(o ? ZE_OPEN : ZE_OK);
}
