# ifndef yySystem
# define yySystem

/* $Id: System.h,v 1.3 1992/02/04 14:01:39 grosch rel $ */

/* $Log:
 */

/* Ich, Doktor Josef Grosch, Informatiker, Jan. 1992 */

# ifdef __STDC__
# define ARGS(parameters)	parameters
# else
# define ARGS(parameters)	()
# endif

/* interface for machine dependencies */

# ifndef bool
# define bool char
# endif
# define tFile int

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

# endif
