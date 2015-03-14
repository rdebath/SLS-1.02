/* $Id: System.c,v 1.7 1992/09/24 13:05:19 grosch rel $ */

/* $Log:
 */

/* Ich, Doktor Josef Grosch, Informatiker, Jan. 1992 */

/* interface for machine dependencies */

/* compilation with the option -DUNIX uses UNIX system calls for IO (efficient),
   otherwise the C library routines are used for IO (portable).			*/

# ifdef __STDC__
# define ARGS(parameters)	parameters
# else
# define ARGS(parameters)	()
# endif

# ifndef bool
# define bool char
# endif
# define tFile int

# ifdef m68000
# define hz 50
# else
# define hz 60
# endif

/* binary IO */

extern tFile	OpenInput	ARGS((char * FileName));
			/* Opens the file whose name is given by the	*/
			/* string parameter 'FileName' for input.	*/
			/* Returns an integer file descriptor.		*/

extern tFile	OpenOutput	ARGS((char * FileName));
			/* Opens the file whose name is given by the	*/
			/* string parameter 'FileName' for output.	*/
			/* Returns an integer file descriptor.		*/

extern int	Read		ARGS((tFile File, char * Buffer, int Size));
			/* Reads 'Size' bytes from file 'tFile' and	*/
			/* stores them in a buffer starting at address	*/
			/* 'Buffer'.					*/
			/* Returns the number of bytes actually read.	*/

extern int	Write		ARGS((tFile File, char * Buffer, int Size));
			/* Writes 'Size' bytes from a buffer starting	*/
			/* at address 'Buffer' to file 'tFile'.		*/
			/* Returns the number of bytes actually written.*/

extern void	Close		ARGS((tFile File));
			/* Closes file 'tFile'.				*/

extern bool IsCharacterSpecial	ARGS((tFile File));
			/* Returns TRUE when file 'tFile' is connected	*/
			/* to a character device like a terminal.	*/


/* calls other than IO */

extern char *	SysAlloc	ARGS((long ByteCount));
			/* Returns a pointer to dynamically allocated	*/
			/* memory space of size 'ByteCount' bytes.	*/
			/* Returns NIL if space is exhausted.		*/

extern long	Time		();
			/* Returns consumed cpu-time in milliseconds.	*/

extern int	GetArgCount	();
			/* Returns number of arguments.			*/

extern void	GetArgument	ARGS((int ArgNum, char * Argument));
			/* Stores a string-valued argument whose index	*/
			/* is 'ArgNum' in the memory area 'Argument'.	*/

extern void	PutArgs		ARGS((int Argc, char * * Argv));
			/* Dummy procedure that passes the values	*/
			/* 'argc' and 'argv' from Modula-2 to C.	*/

extern int	ErrNum		();
			/* Returns the current system error code.	*/

extern int	System		ARGS((char * String));
			/* Executes an operating system command given	*/
			/* as the string 'String'. Returns an exit or	*/
			/* return code.					*/

extern void	Exit		ARGS((int Status));
			/* Terminates program execution and passes the	*/
			/* value 'Status' to the operating system.	*/

extern void	BEGIN_System	();
			/* Dummy procedure with empty body.		*/


# ifndef UNIX

# include <stdio.h>
# define NOFILES 32

static char IsLineBuffered [NOFILES] = { 1, 1, 1, };

static FILE *	FileStore [NOFILES] = {
   stdin, stdout, stderr, NULL, NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
};

static tFile FileToInt (File)
   FILE *	File;
{
   register int	f = fileno (File);
   FileStore [f] = File;
   return f;
}

static FILE * IntToFile (File)
   tFile	File;
{
   return FileStore [File];
}

# endif

/* binary IO */

# include <fcntl.h>
# include <sys/types.h>
# include <sys/stat.h>

bool IsCharacterSpecial (File) tFile File;
{
   struct stat	buf;
   (void) fstat (File, & buf);
   return (0020000 & buf.st_mode) == 0020000;
}

tFile OpenInput (FileName)
   char *	FileName;
{
# ifndef UNIX
   FILE * FilePtr;
   tFile File;
# ifdef MSDOS
   int i, l = strlen (FileName);
   for (i = 0; i < l; i ++) if (FileName [i] == '/') FileName [i] = '\\';
   FilePtr = fopen (FileName, "rb");
# else
   FilePtr = fopen (FileName, "r");
# endif
   if (FilePtr == NULL) return -1;
   File = FileToInt (FilePtr);
   IsLineBuffered [File] = IsCharacterSpecial (File);
   return File;
# else
   return open (FileName, O_RDONLY);
# endif
}

tFile OpenOutput (FileName)
   char *	FileName;
{
# ifndef UNIX
# ifdef MSDOS
   FILE * FilePtr = fopen (FileName, "wb");
# else
   FILE * FilePtr = fopen (FileName, "w");
# endif
   return FilePtr == NULL ? -1 : FileToInt (FilePtr);
# else
   return creat (FileName, 0666);
# endif
}

int Read (File, Buffer, Size)
   tFile	File;
   char *	Buffer;
   int		Size;
{
# ifndef UNIX
   if (IsLineBuffered [File]) {
      Buffer [0] = '\0';
      (void) fgets (Buffer, Size, IntToFile (File));
      return strlen (Buffer);
   } else
      return fread (Buffer, 1, Size, IntToFile (File));
# else
   return read (File, Buffer, Size);
# endif
}

int Write (File, Buffer, Size)
   tFile	File;
   char *	Buffer;
   int		Size;
{
# ifndef UNIX
   return fwrite (Buffer, 1, Size, IntToFile (File));
# else
   return write (File, Buffer, Size);
# endif
}

void Close (File)
   tFile	File;
{
# ifndef UNIX
   (void) fclose (IntToFile (File));
# else
   (void) close (File);
# endif
}

/* calls other than IO */

/* # include <malloc.h> */

char * SysAlloc (ByteCount) long ByteCount; { return (char *) malloc ((unsigned) ByteCount); }

# include <sys/times.h>

long Time ()
{
# ifdef MSDOS
   return clock () / 1000;
# else
   struct tms	buffer;
   (void) times (& buffer);
   return (buffer.tms_utime + buffer.tms_stime) * 1000 / hz;
# endif
}

static int	argc;
static char * *	argv;

int GetArgCount ()
{
   return argc;
}

void GetArgument (ArgNum, Argument)
   int		ArgNum;
   char *	Argument;
{
   register int	i = 0;
   for (;; i ++)
      if ((Argument [i] = argv [ArgNum][i]) == '\0') return;
}

void PutArgs (Argc, Argv)
   int		Argc;
   char * *	Argv;
{
   argc = Argc;
   argv = Argv;
}

# include <errno.h>

int ErrNum () { return errno; }

int System (String) char * String; { return system (String); }

extern void exit ();

void Exit (Status) int Status; { exit (Status); }

void BEGIN_System () {}
