# include "utils.h"
# include <ctype.h>
/*
	This file contains basic low level utilities.
*/

int get_term()
/*
   	This routine gets the terminal specific escape sequences
   	and saves them in the appropriate variable for use by
   	tputs() and tgoto()
*/
{
	char bp[1024], termtype[MAXL];
	static char buf[100];
	char *buf_ptr = buf;
	int status;


	initscr();
	if(getenv("TERM") != '\0')
	   (void) strcpy(termtype, getenv("TERM"));

# ifdef TERM_INFO
	setupterm(termtype,1,&status);
	if(status != 1)
	   return(-1);

	KU = key_up;
	KD = key_down;
	KL = key_left;
	KR = key_right;
	
# else TERM_INFO

	if(tgetent(bp,termtype) != 1)
	   return(-1);
	if((KU = tgetstr("ku",&buf_ptr)) == (char *)'\0')
	   return (1);
	if((KD = tgetstr("kd",&buf_ptr)) == (char *)'\0')
	   return (1);
	if((KL = tgetstr("kl",&buf_ptr)) == (char *)'\0')
	   return (1);
	if((KR = tgetstr("kr",&buf_ptr)) == (char *)'\0')
	   return (1);
# endif TERM_INFO
	return(0);
}

outc(c)
char c;
/*
	This routine puts a single character out to the screen
	at the current location.
*/
{
	addch(c);
	refresh();
}

move_csr(x,y)
int x,y;
/*
   	This routine is used to move the cursor to a new position
   	on the screen.
*/
{
	move(y,x);
	refresh();
}

clr_scr()
/*
	This routine clears the screen and leaves the cursor in the
	upper left corner of the screen.
*/
{
	clear();
	refresh();
}

start_rev()
/*
	Start the reverse video mode.
*/
{
	standout();
}

end_rev()
/*
	End the reverse video mode.
*/
{
	standend();
}

print_str(string)
char *string;
/*
	Print an entire string to the screen.
*/
{
	printw("%s",string);
	refresh();
}

hi_lite(string)
char *string;
/*
	Hi-lite a string that will be printed.
*/
{
	start_rev();
	print_str(string);
	end_rev();
}

save_tty() 
/*
	Save Terminal characteristics Reset by call to reset_tty().
*/
{
	savetty();
}

reset_tty() 
/*
	This routine returns the terminal to the same state it
	had before this program was invoked.
*/
{
	resetty();
}

my_crmode()
/*
	Turn on CRmode.
*/
{
	crmode();
}

no_crmode()
/*
	Turn off CRmode.
*/
{
	nocrmode();
}

my_echo()
/*
	Turn echo back on.
*/
{
	echo();
}

no_echo()
/*
	Turn off echo.
*/
{
	noecho();
}

no_cbreak()
/*
	Take the terminal out of cbreak mode.
*/
{
/*
	noraw();
*/
}

my_cbreak()
/*
	Place the Terminal in Cbreak mode.
*/
{
/*
	raw();
*/
}

void quit()
{
	echo();
	clr_scr();			/* Clear screen routine		     */
	move_csr(0,LAST_LINE);
	my_quit();
	nocrmode();
	echo();
	exit(0);
}
	
rm_lf(array)
char *array;
/*	
	Remove Trailing line feed from a line.
*/
{
	if(array[strlen(array)-1] == '\n')
	  array[strlen(array)-1] = '\0';
}

read_str(string,max_length)
char *string;
int max_length;
{
	char *str;
	int length = 0;

	str = string;
	while((*str =my_getchar()) != LF && *str != RETURN && length <= max_length){
	   if(*str ==  DEL  || *str == BS)
	      if(*str != *string){
		 outc(DEL);
		 outc(SPACE);
		 outc(DEL);
		 *str--;
		 length--;
	      }else
		 continue;
	   else{
	      if(length == max_length)
	        outc(0x07);
	      else{
	        outc(*str);
	        *str++;
	        length++;
	      }
	   }
	}
	*str = '\0';
}

clear_array(array)
char *array;
{
	for(;*array != '\0';*array++)
		*array = '\0';
}

int get_input()
{
	char c, buffer[80];
	int ct = 0, len;

	if((len = strlen(KU)-1) < 2)
	  len = 1;
	while((c = my_getchar()) && (ct < len)){
	   if((c == 0x1b) || (ct > 0)){
	     buffer[ct] = c;
	     ct++; 
	   }else
	     return(c);
	}
	buffer[ct] = c;
	buffer[ct+1] = '\0';
	if((strcmp(KU,buffer)) == 0)
	  return(UP);
	else
	  if((strcmp(KD,buffer)) == 0)
	    return(DOWN);
	  else
	    if((strcmp(KL,buffer)) == 0)
	      return(LEFT);
	    else
	      if((strcmp(KR,buffer)) == 0)
	        return(RIGHT);
	      else
	        return('\0');
}

int my_getchar()
{
	return(getch());
}
/*****************************************************************************
 Prepare for Quit Menu, and exit (restore original settings).
 *****************************************************************************/

my_quit()
{
	clr_scr();	/* Clear screen routine		     */
	move_csr(0,LAST_LINE);
/*	my_quit();*/
/*	my_echo();*/
	reset_tty();
	endwin();
/*	exit(0);*/
}

