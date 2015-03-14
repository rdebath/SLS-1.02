# include <ctype.h>
# include "utils.h"
# include "menu.h"

char *do_selection(int);

int check_file();

/*	PSC MENU COPYRIGHT NOTICE

	Part of PSCMenu

	This software is to be considered to be public domain, it
may be copied, modified and parts of it may be used in other programs
as long as this copyright notice remains intact.

	Copyright()   PSC - Plymouth State College
	Written by:   Ted Wisniewski 12-9-1990
 
*/

/*
 *	void box_screen():
 *
 *	Parameters:	None.
 *
 *	Purpose:	Draw a box around the perimeter of the screen 
 *			in reverse video mode.
 *
 *	Returns:	None.
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

void dir_set(menu_ent *menu)
{
	char buf[LINESZ];
	
	if (menu->execdir[0]!='\0')
	{
		getcwd(buf,LINESZ);
		chdir(menu->execdir);
		strcpy(menu->execdir,buf);
	}
}

void box_screen()
{
	int i;
	char buf[256];

	Sprintf(buf,"%80s"," ");
	start_rev();
	move_csr(0,FIRST_LINE);
	print_str(buf);
	move_csr(0,STATUS_LINE);
	print_str(buf);
	for(i=1;i<22;i++){
	   move_csr(0,i);
	   outc(' ');
	   move_csr(79,i);
	   outc(' ');
	}
	end_rev();
}

/*
 *	void disp_menu():
 *
 *	Parameters:	menu -  a pointer to the menu information
 *			n_ent - The number of entries in the menu.
 *
 *	Purpose:	Display the Menu on the screen.
 *
 *	Returns:	None.
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

void disp_menu(menu,n_ent)
menu_ent *menu;
int n_ent;
{
	int ind = 0;
	char tmp[5];
	
	while(ind < n_ent){
	   if(ind == 0){
	     move_csr((CENTER-(strlen(menu[0].desc)/2)),(ind*2) + 3);
	   }else{
	     if(ind > 0){
	       move_csr(NUMCOL,(ind*2) + 4);
	       Sprintf(tmp,"%d",ind);
	       print_str(tmp);
	     }
	     move_csr(DESCOL,(ind*2) + 4);
	   }
	   hi_lite(menu[ind].desc);
	   ind++;
	}
}

/*
 *	static do_help():
 *
 *	Parameters:	menu - pointer to menu information.
 *			num  - the number of entries in the menu.
 *	
 *	Purpose:	Show help on a menu option.
 *
 *	Returns:	None.
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

static do_help(menu,num)
menu_ent *menu;
int num;
{
	char buff[256];

	 Sprintf(buff,"cat %s",menu[num].help_fil);
	 exec_pipe(buff);
}

/*
 *	void do_menu():
 *
 *	Parameters: 	menu  - pointer to menu information.
 *			n_ent - number of menu entries.
 *	
 *	Purpose:	Main work loop, keystrokes interpreted.
 *
 *	Returns:	None
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

void do_menu(menu,n_ent)
menu_ent *menu;
int n_ent;
{
	char buff[LINESZ], inp = '\0', save = '\0', *ptr;
	int tmp, num;

	move_csr(4,20);
	while(inp != 'q') {
	   switch (inp) {
	        case 'c':
		  erase_menu(n_ent);
	          change();
	          disp_menu(menu,n_ent);
	    	break;
		case 'x':
		  erase_menu(n_ent);
		  exec_it();
	          disp_menu(menu,n_ent);
		break;
		case 'd':
		  erase_menu(n_ent);
	          exec_cshell("ls",CONTINUE);
	          disp_menu(menu,n_ent);
	        break;
		case 'P':
		  erase_menu(n_ent);
		  do_printers();
		  disp_menu(menu,n_ent);
		break;
		case '?':
		  Sprintf(buff,"Get help on item %d..%d? ",1,n_ent-1);
		  move_csr(25,20);
		  hi_lite(buff);
		  num = (my_getchar() - '0');
	   	  erase_line(25,20,30);
		  if((num > 0) && (num < n_ent)){
		     if((check_file(menu[num].help_fil)) == -1){
	   	        move_csr(23,23);
	   	        hi_lite("Sorry, There is no help file for this.");
	  	        continue_it(20);
	   	        erase_line(23,23,40);
		     }else{
		        erase_menu(n_ent);
		        do_help(menu,num);
	                disp_menu(menu,n_ent);
		     }
		  }
		break;
		case 'l':
		  erase_menu(n_ent);
	          exec_pipe("ls -l");
	          disp_menu(menu,n_ent);
		break;
		case 12:
		case RE_DRAW:
	  	  clr_scr();
		  setup_screen();
		  disp_menu(menu,n_ent);
		  Fflush(stdin);
		break;
	        case 'm':
		  if(m_flag != TRUE){
		    erase_menu(n_ent);
	  	    (void) read_menu(menu,MAIN_MENU,&n_ent);
	  	    disp_menu(menu,n_ent);
		    m_flag = TRUE;
		  }
	        break;
		case 'L':
		  clr_scr();
		  move_csr(0,LAST_LINE);
		  reset_tty();
		  log_out();
		break;
	        case 'p':
		case LF:
		case RETURN:
		   if(m_flag != TRUE)
		      inp = n_ent-1 + '0';
		default:
	          tmp = inp - '0';
	          if((tmp >= 1) && (tmp < n_ent)){
	            switch(menu[tmp].key){
	               case '+':
			  erase_menu(n_ent);
#define dirset(num) dir_set(&menu[tmp]) 
			  dirset(tmp);
	  		  exec_pipe(menu[tmp].cmd);
			  dirset(tmp);
	  		  disp_menu(menu,n_ent);
	               break;
	               case '%':
			  dirset(tmp);
			  exec_cshell(menu[tmp].cmd,NO_CONTINUE);
			  dirset(tmp);
	  		  disp_menu(menu,n_ent);
	               break;
	               case '*':
			  dirset(tmp);
			  exec_cshell(menu[tmp].cmd,CONTINUE);
			  dirset(tmp);
	  		  disp_menu(menu,n_ent);
	               break;
		       case '@': menu->key = '@';
	               case '&':
			  erase_menu(n_ent);
	  		  (void) read_menu(menu,menu[tmp].cmd,&n_ent);
	  		  disp_menu(menu,n_ent);
	               break;
	               case '1':
			  erase_menu(n_ent);
			  dirset(tmp);
			  One_fname(buffer,2);
			  if((strlen(buffer)) != 0){
#define GetArgs(buffer) if (!strstr(menu[tmp].cmd,"%s")) Sprintf(string,"%s %s",menu[tmp].cmd,buffer); \
else Sprintf(string,menu[tmp].cmd,buffer);
			   GetArgs(buffer);
			  /*  Sprintf(string,"%s %s",menu[tmp].cmd,buffer); */
			    exec_cshell(string,CONTINUE);
			    clear_array(string);
			  }else
			    erase_line(2,2,40);
			  dirset(tmp);
	  		  disp_menu(menu,n_ent);
	               break;
	               case '2':
			  erase_menu(n_ent);
			  dirset(tmp);
			  One_fname(buffer,2);
			  if((strlen(buffer)) != 0){
			    GetArgs(buffer);
			    /* Sprintf(string,"%s %s ",menu[tmp].cmd,buffer); */
			    Two_fname(buffer,3);
			    if((strlen(buffer)) != 0){
			      if(!isprint(buffer[strlen(buffer)]))
			         rm_lf(buffer);
			      Strcat(string," ");
			      Strcat(string,buffer);
			      exec_cshell(string,CONTINUE);
			      clear_array(string);
			    }else{
			      erase_line(2,2,40);
			      erase_line(2,3,40);
			    }
			  }else
			    erase_line(2,2,40);
			  dirset(tmp);
	  		  disp_menu(menu,n_ent);
	               break;
	               case '3':
			  erase_menu(n_ent);
			  dirset(tmp);
			  ask_who(buffer,2);
			  if((strlen(buffer)) != 0){
			    GetArgs(buffer);
			    /* Sprintf(string,"%s %s",menu[tmp].cmd,buffer); */
			    exec_cshell(string,CONTINUE);
			    clear_array(buffer);
			    clear_array(string);
			  }else
			    erase_line(2,2,40);
			  dirset(tmp);
	  		  disp_menu(menu,n_ent);
	               break;
	               case '4':
			  erase_menu(n_ent);
			  dirset(tmp);
			  ask_what(buffer,2);
			  if((strlen(buffer)) != 0){
			    GetArgs(buffer);
			    /* Sprintf(string,"%s %s",menu[tmp].cmd,buffer); */
			    exec_cshell(string,CONTINUE);
			    clear_array(buffer);
			    clear_array(string);
			  }else
			    erase_line(2,2,40);
			  dirset(tmp);
	  		  disp_menu(menu,n_ent);
	               break;
		       case '5':
			  erase_menu(n_ent);
			  dirset(tmp);
			  ptr = do_selection(menu[tmp].AllowWander);
			  if(ptr != NULL){
			    GetArgs(ptr);
			    /* Sprintf(string,"%s %s",menu[tmp].cmd,ptr); */
			    exec_cshell(string,CONTINUE);
			    clear_array(buffer);
			    clear_array(string);
			  }
			  dirset(tmp);
			  disp_menu(menu,n_ent);
		       break;
		       case '6':
			  erase_menu(n_ent);
			  dirset(tmp);
			  ptr = do_selection(menu[tmp].AllowWander);
			  if(ptr != NULL){
			    GetArgs(ptr);
			    /* Sprintf(string,"%s %s",menu[tmp].cmd,ptr); */
			    exec_pipe(string);
			    clear_array(buffer);
			    clear_array(string);
			  }
			  dirset(tmp);
			  disp_menu(menu,n_ent);
		       break;
	               default:
		       break;
	     }
	   }
	  }
	  move_csr(0,23);
	  inp = my_getchar();
	  if(inp == 'h'){
	     inp = disp_help();
	     if(inp != 'x' && inp!= 'd' && inp != 'c' && inp!=RE_DRAW)
	       disp_menu(menu,n_ent);
	  }
	}
	quit();
}

/*
 *	static erase_menu():
 *
 *	Parameters:	num - the number of entries that need
 *			to be erased.
 *	
 *	Purpose:	Erase the current menu off the screen.
 *
 *	Returns:	None
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

static erase_menu(num)
int num;
{
	int ind = 0;
	char buff[50];
	
	Sprintf(buff,"%40s"," ");
	move_csr(20,3);
	print_str(buff);
	for(ind=0;ind<num;ind++){
	   move_csr(NUMCOL,(ind*2) + 4);
	   print_str(buff);
	}
}

/*
 *	static display_help():
 *
 *	Parameters: 	None
 *	
 *	Purpose:	Display the Help Window.
 *
 *	Returns:	None
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

static disp_help()
{
	char buff[40];
	int i;
	char Key;
	
	Sprintf(buff,"%30s"," ");
	move_csr(25,3);
	hi_lite(buff);
	for(i=4;i<=19;i++){
	   move_csr(25,i);
	   print_str(buff);
	   start_rev();
	   move_csr(25,i);
	   outc(' ');
	   move_csr(54,i);
	   outc(' ');
	   end_rev();
	}
	move_csr(25,19);
	print_str(buff);
	move_csr(25,19);
	hi_lite(buff);
	move_csr(27,4);
	print_str("Command Summary:");
	move_csr(27,6);
	print_str("(q)       Quit Menu");
	move_csr(27,7);
	print_str("(x)       Execute Command");
	move_csr(27,8);
	print_str("(c)       Change Directory");
	move_csr(27,9);
	print_str("(d)       Show Directory");
	move_csr(27,10);
	print_str("(l)       Long Directory");
	move_csr(27,11);
	print_str("(?)       Get Help on Item");
	move_csr(27,12);
	print_str("(m)       Go to Main Menu");
	move_csr(27,13);
	print_str("(p)       Go to Prev Menu");
	move_csr(27,14);
	print_str("(P)       Print a file.");
	move_csr(27,15);
	print_str("(L)       Log off!");
	move_csr(27,16);
	print_str("(ctrl-R)  Redraw Screen.");
	move_csr(27,18);
	Key = my_getchar();
	erase_help();
	return(Key);
}

/*
 *	static erase_help():
 *
 *	Parameters:	None
 *	
 *	Purpose:	Erase The Help Window.
 *
 *	Returns:	None
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

static erase_help()
{
	char buff[40];
	int i;

	Sprintf(buff,"%30s"," ");
	for(i=3;i<=19;i++){
	   move_csr(25,i);
	   print_str(buff);
	}
}

/*
 *	void chop_str():
 *
 *	Parameters:	instring   - A string to chop to 70 columns
 *		        max_length - max length for string.
 *	
 *	Purpose:	Strip anything after the 70th column off.
 *
 *	Returns:	None.
 *
 *	Last Modify:	06-10-91 (TW)
 *
 */

void chop_str(instring,max_length)
char *instring;
int max_length;
{
	int length,i;
	
	length = strlen(instring);
	while(*instring != '\0')
	   *instring++;
	for(i=length;i>max_length;i--,*instring--)
	  *instring = '\0';
}

void pad_str(instring)
char *instring;
{
	int length,i;
	
	length = strlen(instring);
	for(i=length;i<=70;i++)
	   instring[i] = ' ';
	instring[70] = '\0';
}

/*
 *	static do_help():
 *
 *	Parameters:	name - String containing a file name.
 *			y    - Number of lines from top of screen.
 *	
 *	Purpose:	read a file name into the string.
 *
 *	Returns:	None.
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

static One_fname(name,y)
char *name;
int y;
{
	move_csr(2,y);
	clear_array(name);
	print_str("Filename: ");
	read_str(name,30);
}

/*
 *	static Two_fname():
 *
 *	Parameters:	name - String containing a file name.
 *			y    - Number of lines from top of screen.
 *	
 *	Purpose:
 *
 *	Returns:
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

static Two_fname(name,y)
char *name;
int y;
{
	move_csr(2,y);
	clear_array(name);
	print_str("New File: ");
	read_str(name,30);
}

/*
 *	static change():
 *
 *	Parameters:	None.
 *	
 *	Purpose:	Change to a new working directory.
 *
 *	Returns:	None.
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

static change()
{
	move_csr(2,2);
	clear_array(buffer);
	print_str("Directory: ");
	read_str(buffer,30);
	(void) chdir(buffer);
	erase_line(2,2,60);
	prt_curdir();
	clear_array(buffer);
}

/*
 *	static exec_it():
 *
 * 	Parameters:	None.
 *	
 *	Purpose:	Execute A shell commenad.
 *
 *	Returns:	None.
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

static exec_it()
{
	move_csr(2,2);
	print_str("Command: ");
	clear_array(buffer);
	read_str(buffer,30);
	exec_cshell(buffer,CONTINUE);
}

/*
 *	static ask_who():
 *
 *	Parameters:	None.
 *	
 *	Purpose:	Ask for a login name.
 *
 *	Returns:	None.
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

static ask_who(name,y)
char *name;
int y;
{
	move_csr(2,y);
	clear_array(name);
	print_str("To Whom: ");
	read_str(name,30);
	move_csr(2,3);
	Fflush(stdout);
}

/*
 *	static ask_what():
 *
 *	Parameters:	name - Name of what you are asking.
 *			y    - number of rows down from top of screen.
 *	
 *	Purpose:	Ask for a topic.
 *
 *	Returns:	None.
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

static ask_what(name,y)
char *name;
int y;
{
	move_csr(2,y);
	clear_array(name);
	print_str("Topic: ");
	read_str(name,30);
}

/*
 *	void check_dirname():
 *
 *	Parameters:	string - Name of directory.
 *	
 *	Purpose:	Expand '~' to users Home Directory.
 *
 *	Returns:	None.
 *
 *	Last Modify:	01-12-91 (TW)
 *
 */

void check_dirname(string)
char *string;
{
	char *tmp_str;

	tmp_str = (char *) malloc(80);;
	if(*string == '~'){
	  Strcpy(tmp_str,getenv("HOME"));
	  Strcat(tmp_str,string+1);
	  Strcat(tmp_str,"/");
	  string = tmp_str;
	}
}
