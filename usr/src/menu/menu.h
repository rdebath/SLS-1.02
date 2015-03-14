/*	PSC MENU COPYRIGHT NOTICE

	Part of PSCMenu

	This software is to be considered to be public domain, it
may be copied, modified and parts of it may be used in other programs
as long as this copyright notice remains intact.

	Copyright()   PSC - Plymouth State College
	Written by:   Ted Wisniewski 12-9-1990
 
*/
#include <curses.h>
#include <unistd.h>

# define 	FOURK		4096
# define 	ONEK		1024
# define	BUF_SIZ		256
# define	LINESZ		90
# define 	FIRST_LINE	0
# define	STATUS_LINE	22
# define	NUMCOL		22
# define	DESCOL		25
# define 	LAST_LINE	23
# define 	CONT_LINE	21
# define 	N_ENTRIES	9
# define 	RE_DRAW		('r' & 037)
# define 	MAIN_MENU	"main.menu"
# define	MENU_DIR	"/usr/local/lib/menus/"
# define	RETURN		0x0d
# define	LF		0x0a
# define	SPACE		0x20

# ifndef TRUE
# define	TRUE		1
# define	FALSE		0
#endif TRUE

# define 	CENTER		40
# define 	HEADER		"LINUX Menu System"
# define	CMD_LIN		"h)elp q)uit"
# define	CONTINUE	1
# define	NO_CONTINUE	0

# define 	Sprintf		(void) sprintf
# define	Strcpy		(void) strcpy
# define	Strcat		(void) strcat
# define	Fprintf		(void) fprintf
# define	Fflush		(void) fflush
# define	Fputc		(void) fputc
# define	Fputs		(void) fputs
# define	Fgets		(void) fgets
# define	Pclose		(void) pclose

# ifndef HPUX
# define	Getwd		(void) getwd
# else
# define	Getwd		(void) getcwd
# endif HPUX

# define	Wait		(void) wait
# define	Execl		(void) execl

int catch();

char *strcat(), *strcpy();

typedef struct
{
	char key;		/* Key character for command types.  */
	char desc[LINESZ];	/* Description of command.	     */
	char **script;		/* Strings to make a shell script.   */
	char cmd[LINESZ];  	/* The Command.                      */
        char help_fil[LINESZ];  /* Name of the Help file for option. */
        char execdir[LINESZ];   /* Name of the dir to cd to before exec. */
	int  AllowWander;	/* Let change dir in visual */
}menu_ent;

menu_ent main_menu[N_ENTRIES];

typedef int FLAG;

FLAG m_flag;


char line[BUF_SIZ], buffer[BUF_SIZ], string[BUF_SIZ];
char menu_dir[BUF_SIZ];

int get_term();
