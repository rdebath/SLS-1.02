# include <stdio.h>
# include <sys/file.h>
# include <unistd.h>
# include "menu.h"

/*	PSC MENU COPYRIGHT NOTICE

	Part of PSCMenu

	This software is to be considered to be public domain, it
may be copied, modified and parts of it may be used in other programs
as long as this copyright notice remains intact.

	Copyright()   PSC - Plymouth State College
	Written by:   Ted Wisniewski 12-9-1990
 
*/

/*
 *	int check_file(instring)
 *
 *	Parameters:	instring - The Name of the file to be accessed.
 *
 *	Returns:	-1  = Failure, file may not exist.
 *			1   = File is Readable.
 *
 * 	Purpose:	Determine if a file is readable.
 *
 *	Last Modify:	3-31-91 (TW)
 */
int check_file(instring)
char *instring;
{
	if(strlen(instring) > 0){
	  if((access(instring,R_OK)) == -1)
	    return(-1);
	  else
	    return(1);
	}else
	  return(-1);
}

/*
 *	read_menu(menu,file_name,num)
 *
 *	Parameters:	menu 		- Pointer to the menu to be loaded.
 *			file_name	- Name of the menu file.
 *			num 		- The number of menu entries read.
 *
 *	Returns:	None.
 *
 *	Purpose:	Read a menu from file information.
 *	
 *	Last Modify:	3-31-91 (TW)
 */

void read_menu(menu,file_name,num)
menu_ent *menu;
char *file_name;
int *num;
{
	FILE *fp;
	int index = 0;
	char buff[BUF_SIZ];
	
	if(menu->key == '@')
	   m_flag = TRUE;
	else
	   m_flag = FALSE;
	Sprintf(buff,"%s%s",menu_dir,file_name);
	if((access(buff,R_OK|F_OK)) == -1){
	   move_csr(26,11);
	   hi_lite("Menu file cannot be accessed.");
	   continue_it(20);
	   erase_line(26,11,30);
	}else{
	   fp = fopen(buff,"r");
	   menu[index].execdir[0] = 0;
	   menu[index].AllowWander = 1;
	   while(!feof(fp)){
	      Fgets(buff,256,fp);
	      rm_lf(buff);
	      if(!feof(fp) && (index <= 7)){
		switch (buff[0]){
		  case '$':	/* Menu Header */
			menu[index].key = buff[0];
			Strcpy(menu[index].desc,buff+2);
			index++;
			menu[index].execdir[0] = 0;
			menu[index].AllowWander = 1;
		  break;
		  case '?': 	/* Menu Comment (User sees)     */
			Strcpy(menu[index].desc,buff+2);
			chop_str(menu[index].desc,36);
			index++;
			menu[index].execdir[0] = 0;
			menu[index].AllowWander = 1;
		  break;
		  case '#':	/* Help file name 		*/
			Strcpy(menu[index].help_fil,buff+2);
		  break;
		  case '+':	/* Exec in pipe			*/
			menu[index].key = buff[0];
			Strcpy(menu[index].cmd,buff+2);
		  break;
		  case '!':		/* Strings used to build a bourne*/
					/* shell script to be run.	 */
					/* Unimplemented as of yet.	 */
	          break;
		  case '%':		/* '%' Do not use continue line  */
		  case '*':		/* This command to be executed   */
					/* in a sub-shell. Use Continue  */
			menu[index].key = buff[0];
			Strcpy(menu[index].cmd,buff+2);
		  break;
		  case '@':		/* This Menu is a main Menu      */
		  case '&':		/* This Menu is a regular Menu.  */
			menu[index].key = buff[0];
			Strcpy(menu[index].cmd,buff+2);
		  break;
		  case '1':		/* Ask for a file name		*/
			menu[index].key = buff[0];
			Strcpy(menu[index].cmd,buff+2);
		  break;
		  case '2':		/* Ask for two file Names	*/
			menu[index].key = buff[0];
			Strcpy(menu[index].cmd,buff+2);
		  break;
		  case '3':		/* Ask Whom			*/
			menu[index].key = buff[0];
			Strcpy(menu[index].cmd,buff+2);
		  break;
		  case '4':		/* Ask what?			*/
			menu[index].key = buff[0];
			Strcpy(menu[index].cmd,buff+2);
		  break;
		  case '5':		/* Use visual directory in shell */
			menu[index].key = buff[0];
			Strcpy(menu[index].cmd,buff+2);
		  break;
		  case '6':		/* Use visual in pipe		*/
			menu[index].key = buff[0];
			Strcpy(menu[index].cmd,buff+2);
		  break;
		  case 'd':		/* Set working directory for command */
			Strcpy(menu[index].execdir,buff+2);
			if (strlen(menu[index].execdir)!=0)
				menu[index].AllowWander = ('/' != menu[index].execdir[
				  strlen(menu[index].execdir)-1]);
		  break;
		}
	      }
	    }
	(void) fclose(fp);
	*num = index;
	}
}
