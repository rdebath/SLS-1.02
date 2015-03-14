# include <stdio.h>
# include <sys/file.h>
# include <sys/ioctl.h>
# ifdef HPUX
# include <sys/bsdtty.h>
# endif HPUX
# include "menu.h"

/*	PSC MENU COPYRIGHT NOTICE

	Part of PSCMenu

	This software is to be considered to be public domain, it
may be copied, modified and parts of it may be used in other programs
as long as this copyright notice remains intact.

	Copyright()   PSC - Plymouth State College
	Written by:   Ted Wisniewski 12-9-1990
 
*/

/*****************************************************************************
	Execute a process and have the output read into a buffer and then
	print it to the screen.
 *****************************************************************************/

exec_pipe(comline)
char *comline;
{
	char inbuf[LINESZ], list[FOURK][80];
	FILE *f, *popen();
	int num = 1, length = 0;

	move_csr(5,2);
	print_str("Working...");
	if((f = popen(comline,"r")) == NULL){
	   Strcpy(list[0],"Command not found");
	}else
	   while(fgets(inbuf,LINESZ,f) != NULL){
	      rm_lf(inbuf);
	      if(strlen(inbuf) > 70)
	        chop_str(inbuf,70);
	      else
	        pad_str(inbuf);
	      Strcpy(list[num],inbuf);
	      if(strlen(inbuf) > length)
		length = strlen(inbuf);
	      num++;
	   }
	   clr_area(2,5,1,15);
	   print_output(list,num,length);
	Pclose(f);
}

/**************************************************************************
 Print output to the Menu Window,  when done clear the Window.
 **************************************************************************/

print_output(output,number,length)
char output[FOURK][80];
int number, length;
{
	int i, k = 4, left;

	left = number;
	for(i=1;i<number;++i){
	   move_csr(5,k);
	   print_str(output[i]);
	   k++;
	   if(i%15 == 14){
	     left -= 15;
	     if(cont_it() == 1){
	        clr_area(5,4,(k-4),length+2);
	        return;
	     }
/*	     clr_area(5,4,(k-4),length+2);*/
	     move_csr(5,4);
	     k = 4;
	   }
	   Fflush(stdout);
	}
	if(left < 15)
	  clr_area(5+left,5,(15-left),length+2);
	continue_it(CONT_LINE);
	clr_area(5,4,(k-4),length+2);
}

/**************************************************************************
 Request input from the user as to whether he/she wishes to continue.
 This routine gives the option to stop seeing output.
 **************************************************************************/

cont_it()
{
	char c;

	move_csr(20,CONT_LINE);
	hi_lite("Press space to continue or \'q\' to stop.");
	c = my_getchar();
	while(c != 'q' && c != ' ')
	   c = my_getchar();
	erase_line(20,CONT_LINE,45);
	if(c == 'q')
	   return 1;
	else
	   return 0;
}

/**************************************************************************
 Request input from the user as to whether he/she wishes to continue.
 **************************************************************************/

continue_it(line_num)
int line_num;
{
	move_csr(27,line_num);
	hi_lite("Press a space to Continue.");
	while(my_getchar() != SPACE);
	erase_line(27,line_num,45);
}

/*****************************************************************************
 Print The current working directory at the bottom of the screen.
 *****************************************************************************/


prt_curdir()
{
      char direct[BUF_SIZ], *getcwd();
	int i;

	move_csr(0,STATUS_LINE);
	hi_lite("Directory:");
	getcwd(direct,BUF_SIZ);
	if(strlen(direct) < 40)
	   for(i=strlen(direct);i<=40;i++)
	      direct[i]=' ';
	else
	   for(i=40;i<=80;i++)
   	      direct[i+1] = direct[i] = '\0';
	move_csr(12,STATUS_LINE);
	hi_lite(direct);
}

/*****************************************************************************
 Clear an Area in the Menu Window.
 *****************************************************************************/

clr_area(st_y,st_x,n_line,length)
int st_y,st_x,n_line,length;
{
	int i;

	for(i=0;i<=length;i++)
	  line[i] = ' ';
	line[i+1] = '\0';
	for(i=(st_y-1);i<(n_line+st_y);i++){
	   move_csr(st_x,i);
	   print_str(line);
	}
}

/*****************************************************************************
 Erase a line within the Menu Window.
 *****************************************************************************/

erase_line(x,y,length)
int x,y,length;
{
	int i;

	for(i=0;i<=length;i++)
	    line[i] = ' ';
	line[i+1] = '\0';
	move_csr(x,y);
	print_str(line);
}

/*****************************************************************************
 Execute a shell command.  Output does not come to back to the window.
 *****************************************************************************/

exec_cshell(command,flag)
char *command;
int flag;
{
	clr_scr();
	Fflush(stdout);
	if(fork() == 0){
	  reset_tty(); 
	  my_echo();
	  no_crmode();
	  no_cbreak();
	  Execl("/bin/sh","sh","-c",command,0);
	}else
	  Wait((int *)0);
	setup_tty();
	if(flag)
	  continue_it(LAST_LINE);
	setup_screen();
}

/*****************************************************************************
 Draw borders and print Header on the screen.
 *****************************************************************************/

setup_screen()
{
	initscr();
	clr_scr();			/* Clear screen routine		     */
	box_screen();
	move_csr((CENTER-strlen(HEADER)/2),FIRST_LINE);
	hi_lite(HEADER);
	move_csr(68,STATUS_LINE);
	hi_lite(CMD_LIN);
	prt_curdir();
}

/*****************************************************************************
 Make sure The user does not try to run menu in the background.
 *****************************************************************************/

check_run()
{
        int     tpgrp;  /* terminal control process group */

        if (ioctl(2, TIOCGPGRP, &tpgrp) != 0) { /* hope stderr open */
                exit(1);
        }
	if(getpgrp() != tpgrp){
	  Fprintf(stderr,"\nSorry, Cannot run in background.\n");
	  exit(0);
	}
}

/*****************************************************************************
 Make sure Output goes to the screen and terminal is capable of running
 menu.
 *****************************************************************************/

initialize()
{
	int term;

	if(!isatty(1)){
	   Fprintf(stderr,"Sorry, Cannot output to the screen (exiting).\n");
	   exit(4);
	}
	if((term = get_term()) == -1){
	   Fprintf(stderr,"Sorry, Bad termcap entry (exiting).\n");
	   exit(4);
	}
	save_tty();			/* Save tty states		     */
	setup_tty();
}

/*****************************************************************************
 Set proper terminal settings for use while within menu.
 *****************************************************************************/

setup_tty()
{
	no_echo();
	my_cbreak();
	my_crmode();
}

int main(argc,argv)
int argc;
char *argv[];
{
	int num;

	    /************************************************************
		If More than two arguments are supplied print the
		Usage statement.
	     ************************************************************/
	if(argc > 2){
	  Fprintf(stderr,"Usage: menu\n");
	  Fprintf(stderr,"Usage: menu [menu_directory]\n");
	  exit(0);
	}else{
	    /************************************************************
		If there are no additional arguments Use the default
		Menu directory.
	     ************************************************************/
	  if(argc == 1)
	     Strcpy(menu_dir,MENU_DIR);
	  else
	    /************************************************************
		See if the directory name ends in a '/' if not add
		one.
	     ************************************************************/
	    if(argc == 2){
	       Strcpy(menu_dir,argv[1]);
	       check_dirname(menu_dir);
	       if(menu_dir[strlen(menu_dir)-1] != '/')
	          Strcat(menu_dir,"/");
	    }
	    Sprintf(buffer,"%s%s",menu_dir,MAIN_MENU);
	    /************************************************************
	        Check to make sure the menu directory is Read-able and
		executeable
	     ************************************************************/
	    if(access(menu_dir,R_OK|X_OK)){
	       Fprintf(stderr,"Directory :%s: is not accessable ",menu_dir);
	       Fprintf(stderr,"or does not exist.\n");
	       exit(1);
	    }
	    /************************************************************
	        Check to see if the main menu file exists, if it does 
	        not exist, exit the program.
	     ************************************************************/
	    if(access(buffer,F_OK)){
	       Fprintf(stderr,"Menu files do not exist in %s.\n",menu_dir);
	       (void) exit(2);
	    }
	}
# ifndef SYSTEM_FIVE
	(void) setpgrp(getpid(),0);
# else
	(void) setpgrp();
# endif SYSTEM_FIVE
	check_run();
	initialize();		
	setup_screen();
	setup_sigs();
	main_menu->key = '@';			/* We start in main menu */
	(void) read_menu(main_menu,MAIN_MENU,&num);
	disp_menu(main_menu,num);
	Fflush(stdin);
	do_menu(main_menu,num);
	/* return(0); */
	my_quit();
}
