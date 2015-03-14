#line 1 "comment5.d"
/* COMMENT.D */
/* dient zum Umwandeln von Kommentaren: */
/* Inputzeile: */
/*      text # comment */
/* Outputzeile: */
/*      text / * comment * / */
/* Bruno Haible 9.7.1990, 6.8.1990, 15.8.1990, 22.9.1990, 17.1.1991, 31.8.1991 */

/* Aufrufmöglichkeiten, um program.d in program.c umzuwandeln: */
/*   comment program */
/*   comment           und dann von Hand 'program' eingeben */
/*   comment program.d program.c */
/*   comment program.d > program.c */

/* Methode: */
/* Das Inputfile wird Zeile für Zeile umgewandelt. Ein '#', gefolgt von ' ', */
/* leitet einen Kommentar ein, der bis Zeilenende geht. */
/* '#'' ' wird durch '/''*'' ' ersetzt, und vors Zeilenende kommt ' ''*''/'. */
/* Ein '\' unmittelbar am Ende einer Zeile wird dabei zum Zeilenende gerechnet. */
/* Als erste Zeile wird  '#line 1 "sourcefile"' eingefügt. */

/* Syntax eines Inputfile:                                               # */
/* -------> Char ----> '#' -> ' ' ----> Char ----> '\\' ---> '\n' -----> # */
/*    / /         \                 /         \ \       /           \    # */
/*   |  \         /                 \         /  --->---             |   # */
/*    \  --<------                   --<------                      /    # */
/*     -------------------<-----------------------------------------     # */

#include <stdio.h>
#ifdef LATTICE
  #define ATARI
  #include <osbind.h> /* für Malloc */
  long _MNEED = 1024; /* brauche keinen zusätzlichen Speicher */
  long _STACK = 4096; /* brauche ca. 4K C-Stack */
  #define fopen_read_ascii  "ra"
  #define fopen_write_ascii  "wa"
#endif
#ifdef __TURBOC__
  #define ATARI
  #include <stdlib.h> /* für exit */
  #include <tos.h>    /* für Malloc */
  #define fopen_read_ascii  "r"
  #define fopen_write_ascii  "w"
#endif
#ifdef __EMX__
  #define unix
#endif
#if defined(unix) || defined(__unix)
  #define fopen_read_ascii  "r"
  #define fopen_write_ascii  "w"
  #define fputc  putc
  #define fgetc  getc
#endif
#ifdef vms
  #define fopen_read_ascii  "r"
  #define fopen_write_ascii  "w"
#endif

typedef unsigned char  Char;

int main(argc,argv)
 int    argc;
 char** argv;
{ char infilenamebuffer[1000];
  char outfilenamebuffer[1000];
  FILE * infile;
  FILE * outfile;
  if( argc==3 ){
    /* Aufruf der Form 'comment source destination' */
    { char* p1 = argv[1]; char* p2 = infilenamebuffer; while (*p2++ = *p1++); }
    { char* p1 = argv[2]; char* p2 = outfilenamebuffer; while (*p2++ = *p1++); }
    }
  else
    { char filenamebuffer[1000];
      char* filename;
      if( argc==2 ){ filename=argv[1]; }
        else { printf("Filename: "); filename=gets(filenamebuffer); }
      /* infilename basteln: filename+".d" */
      { char* p = infilenamebuffer;
        { char* p2 = filename; char c; while (c = *p2++) { *p++ = c; } }
        /* Endet filename bereits mit ".d" ? */
        if ((&p[-2] >= infilenamebuffer) && (p[-2]=='.') && (p[-1]=='d'))
          { *p++ = '\0'; goto to_stdout; } /* ja -> Output nach stdout */
        *p++ = '.'; *p++ = 'd';
        *p++ = '\0';
      }
      /* outfilename basteln: filename+".c" */
      { char* p = outfilenamebuffer;
        { char* p2 = filename; char c; while (c = *p2++) { *p++ = c; } }
        *p++ = '.'; *p++ = 'c';
        *p++ = '\0';
      }
    }
  /* infile öffnen: */
  if( (infile = fopen(infilenamebuffer,fopen_read_ascii))==NULL ){ exit(1); }
  /* outfile öffnen: */
  if( (outfile = fopen(outfilenamebuffer,fopen_write_ascii))==NULL ){
      fclose(infile); exit(1); }
  if (0)
    { to_stdout:
      /* infile öffnen: */
      if( (infile = fopen(infilenamebuffer,fopen_read_ascii))==NULL ){ exit(1); }
      outfile = stdout; /* outfile = Standard-Output */
    }
  #ifdef ATARI
  /* infile und outfile einen größeren Buffer zuweisen: */
  { long free = Malloc(-1); /* Anzahl der freien Bytes */
    long free2 = free/2; /* halbe Anzahl */
    long freep1 = Malloc(free2); /* 1. Pointer auf freien Speicher */
    long freep2 = Malloc(free2); /* 2. Pointer auf freien Speicher */
    if( (freep1<0) || (freep2<0) ){ exit(1); } /* evtl. Fehler */
    setvbuf(infile,(char*)freep1,_IOFBF,free2);
    setvbuf(outfile,(char*)freep2,_IOFBF,free2);
  }
  #endif
  /* Header in outfile schreiben: */
  { fputs("#line 1 \"",outfile);
    fputs(infilenamebuffer,outfile);
    fputs("\"\n",outfile);
  }
  /* infile in outfile kopieren: */
  #define fput_startcomment(outfile)  \
    { fputc('/',outfile); fputc('*',outfile); fputc(' ',outfile); }
  #define fput_endcomment(outfile)  \
    { fputc(' ',outfile); fputc('*',outfile); fputc('/',outfile); }
  { register int c;
    L1:  /* innerhalb einer Zeile, vor Kommentar */
         c = fgetc(infile) ;
    L1a: if( c==EOF ){ goto L3; }
         if( !(c=='#') ){ fputc(c,outfile); goto L1; }
         /* innerhalb einer Zeile, nach '#', vor ' ' */
         c = fgetc(infile) ;
         if( !(c==' ') ){ fputc('#',outfile); goto L1a; }
         fput_startcomment(outfile);
    L2:  /* innerhalb eines Kommentars */
         c = fgetc(infile) ;
    L2a: if( c==EOF ){ fput_endcomment(outfile); goto L3; }
         if( c=='\n' ){ fput_endcomment(outfile); fputc(c,outfile); goto L1; }
         if( !(c=='\\') ){ fputc(c,outfile); goto L2; }
         /* innerhalb eines Kommentars, nach '\\' */
         c = fgetc(infile) ;
         if( !(c=='\n') ){ fputc('\\',outfile); goto L2a; }
         fput_endcomment(outfile); fputc('\\',outfile); fputc(c,outfile);
         goto L1;
    L3:  ; /* am File-Ende */
  }
  /* Files schließen: */
  fclose(infile);
  fclose(outfile);
  exit(0); /* alles OK */
}

