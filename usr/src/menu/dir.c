# include <stdio.h>
# include <sys/types.h>
# include <stdlib.h>

# ifndef HPUX
# include <sys/dir.h>
# else
# include <sys/dirent.h>
# endif HPUX

# include <sys/stat.h>
# include <signal.h>
# include "menu.h"
# include "dir.h"

/*
 *	is_directory(directory,file):  
 *			The first parameter is the name
 *			directory and the second is the
 *			name of the file in the directory.
 *
 *	Purpose:	Determine if a file is a directory file.
 *
 *	Return:		-1 = Failure to stat file, (file may not exist)
 *		        1  = Is a directory file.
 *			0  = Is a regular file.
 *
 *	Last Modify:	3-31-91 (TW)
 */

int is_directory(file_name)
char *file_name;
{
	struct stat st_buf;

	if(stat(file_name,&st_buf) < 0)
	  return(-1);
	else
	  if(st_buf.st_mode & S_IFDIR)
	    return(1);
	return(0);
}

/*
 *	void replace_spaces(string):
 *			The Parameter "string" is a file name that may
 *			contain spaces.  Replace all spaces with the
 *			'*' character.
 *
 *	Purpose:	Make it so a file with spaces in the name may
 *			be accessed.
 *
 *	Returns:	None
 *
 *	Last Modify:	5-01-91 (TW)
 *
 */

void replace_spaces(string)
char *string;
{
	for(;*string != '\0';string++)
	  if(isspace(*string))
	    *string = '*';
}

/*
 *	int get_dir(dir_entries,name,dirs):
 *			The parameter "dir_entries" an character array of
 *			MAX_ENT entries, with each entry being ENT_LEN in
 * 			length. Dirs= let use directories.
 *
 *	Purpose:	Read the current directory file and extract the
 *			file names and sort the list.
 *
 *	Return Values:	-1  = Failed to read the directory file.
 *			ct  = The number of files in the directory, the count
 *			excludes (dot) files and Directory Files.
 *
 *	Last Modify:	3-31-91 (TW)
 *			5-01-91 (TW)
 */

int get_dir(dir_entries,name,dirs)
char dir_entries[MAX_ENT][ENT_LEN];
char *name;
int dirs;
{
	DIR *dirp;
# if defined (SUN) || defined (BSD43)
	struct direct *dp;
# else
# ifdef linux
	struct direct *dp;
# else
	struct dirent *dp;
# endif linux
# endif SUN
	char buff[80];
	int i = 0,ct;
	
	if((dirp = opendir(name)) == (DIR *)NULL)    /* Open the directory */
	  return(-1);				     /* Return with error  */
	for(dp = readdir(dirp);dp != NULL; dp = readdir(dirp)){
/*	   if(dp->d_name[0] != '.' && !is_directory(dp->d_name)){ */
	   if(!dirs && is_directory(dp->d_name))
		continue;
  	   if(strcmp(dp->d_name,".")) {
	      if (is_directory(dp->d_name))
		strcat(dp->d_name,"/");
	      replace_spaces(dp->d_name);
	      memccpy(dir_entries[i],dp->d_name,'\0',ENT_LEN-2);
	      dir_entries[i][ENT_LEN-1] = '*';		     /* Just in case long name*/
	      i++;
	      ct = i;
	   }
	}
	closedir(dirp);				    /* Close the Directory */
	/* qsort(dir_entries,ct,ENT_LEN,strcmp);	*/    /* Sort the List       */
	return(ct);
}

/*
 *	int print_dir(dir_entries,start_ent,n_entries,end,dptr)
 *
 *	Parameters:	dir_entries - The list of files.
 *			start_ent   - Start printing with entry #.
 *			n_entries   - How many entries are there.
 *			end         - What # do we stop printing at.
 *			dptr	    - Special info.
 *	Purpose:	Print a select group of file names ont the screen.
 *
 *	Returns:	The Number of the last entry printed.
 *
 *	Last Modify:	3-31-91 (TW)
 */

int print_dir(dir_entries,start_ent,n_entries,end,dptr)
char dir_entries[MAX_ENT][ENT_LEN];
int start_ent,n_entries,end;
dir_info *dptr;
{
	int i, row = 5, col = 0;
	int this_scr;

	print_other();			/* Print Directions on the top of the*/
					/* Screen.			     */
	dptr->max_row = dptr->max_col = 0;
	this_scr = start_ent + 45;
	if(n_entries > end){
	  if(this_scr > n_entries)
	     this_scr = n_entries;
	}else{
	  this_scr = n_entries;
	}
	/*
	 * Print all for this screen.
         */
	for(i=start_ent;i<this_scr;i++){
	  if((i != 0) && ((i % 15) == 0) && ((i % 45) != 0)){
	     col++;
	     row = 5;
	  }
	  move_csr((col*20)+10,row);
	  print_str(dir_entries[i]);
	  row++;
	  if(dptr->max_row < row-2)
	     dptr->max_row = row-2;
	  if(dptr->max_col < ((col)+1)*20)
	     dptr->max_col = ((col+1)*20);
	  
	}
	return(this_scr);
}

/*
 *	move_entry(dir_entries,row,col,row_off,start,flag)
 *
 *	Parameters:	dir_entries - The list of file Names.
 *			row 	    - Which row we are on.
 *			row_off	    - Row offset from top of screen.
 *			start	    - The array index number of the
 *				      first file name.
 *			flag	    - Boolean: True == Highlight
 *				      False == No Highlight
 *
 *	Purpose:	Allow user to move a high-lite bar to select a
 *			file.
 *
 *	Last Modify:	3-31-91 (TW)
 */

void move_entry(dir_entries,row,col,row_off,start,flag)
char dir_entries[MAX_ENT][ENT_LEN];
int row,col,row_off,start,flag;
{
	int index;

	if(col == 0)
	  index = ((col%13))+row+start;
	else
	  index = (((col/15)*15))+row+start;
        move_csr(col+10,row+row_off);
	if(!flag)
	  print_str(dir_entries[index]);
	else
	  hi_lite(dir_entries[index]);
}

/*
 *	char *select_item(dir_ent,number,start,end,dp)
 *
 *	parameters:	dir_ent - The list of file Names.
 *			number  - 
 *			start   - The Starting Number for the current screen.
 *			end     - The number of the last one on this screen.
 *			dp      - Special Info.
 *
 *	Purpose:	Loop to get selections. Ultimately returns a file name.
 *
 *	Last Modify:	3-31-91 (TW)
 *			5-01-91 (TW)
 */

char *select_item(dir_ent,number,start,end,dp)
char dir_ent[MAX_ENT][ENT_LEN];
int number,start,end;
dir_info *dp;
{
	int row = 0, col = 0, row_off = 5, x;
	int inp;
	char buff[80];

	move_csr(col+10,row_off+col);
	hi_lite(dir_ent[0]);
	for(;;){
	switch(inp = get_input()){
	   case 'n':			/* Go to next page		*/
	   case 'N':
		   if(number > end){
		     start += 45;
		     clr_area(2,2,2,76);
		     clr_area(3,2,dp->max_row,dp->max_col+5);
		     end = print_dir(dir_ent,start,number,end,dp);
		     row = col = 0;
		     move_entry(dir_ent,row,col,row_off,start,1);
		   }
	   break;
	   case 'p':			/* Go to the previous page	*/
   	   case 'P':
		   if(start >= 45){
	     	     start -= 45;
		     clr_area(2,2,2,76);
		     clr_area(3,2,dp->max_row,dp->max_col+5);
		     end = print_dir(dir_ent,start,number,end-45,dp);
		     row = col = 0;
		     move_entry(dir_ent,row,col,row_off,start,1);
		   }
	   break;
	   case 'j':			/* Move the hi-lite bar down 	*/
	   case 'J':
	   case DOWN:
		   if(row != 14){
		     move_entry(dir_ent,row,col,row_off,start,0);
		     row++;
		     move_entry(dir_ent,row,col,row_off,start,1);
		   }
	   break;
	   case 'k':			/* Move The hi-lite bar up	*/
	   case 'K':
	   case UP:
		   if(row != 0){
		     move_entry(dir_ent,row,col,row_off,start,0);
		     row--;
		     move_entry(dir_ent,row,col,row_off,start,1);
		}
	   break;
	   case 'l':			/* Move The hi-lite bar Right	*/
	   case 'L':
	   case RIGHT:
		   if(col < 40){
		     move_entry(dir_ent,row,col,row_off,start,0);
		     col += 20;
		     move_entry(dir_ent,row,col,row_off,start,1);
		}
	   break;
	   case 'h':			/* Move The hi-lite bar Left	*/
	   case 'H':
	   case LEFT:
		   if(col != 0){
		     move_entry(dir_ent,row,col,row_off,start,0);
		     col -= 20;
		     move_entry(dir_ent,row,col,row_off,start,1);
		}
	   break;
	   case '\n':			/* Make a selection		*/
	   case '\r':
		   if(col == 0)
		     x = ((col%14))+row+start;
		   else
		     x = (((col/15)*15))+row+start;
		   if(strlen(dir_ent[x]) >= 1){
		     clr_area(2,2,2,76);
		     clr_area(3,2,dp->max_row,dp->max_col+5);
		     clr_area(20,38,1,38);
		     return(dir_ent[x]);
	 	   }else{
		     clr_area(2,2,2,76);
		     clr_area(3,2,dp->max_row,dp->max_col+5);
		     clr_area(20,38,1,38);
		     return('\0');
		   }
	   break;
	   case 'q':			/* Do Not make a selection	*/
		    clr_area(2,2,2,76);
		    clr_area(3,2,dp->max_row,dp->max_col+5);
		    clr_area(20,38,1,38);
		    return('\0');
	   break;
	   default:
	   break;
	}
     }
}

/*
 *	print_other()
 *	
 *	Parameters:	None
 *
 * 	Purpose:	Print Some Instructions.
 *	
 *	Last Modify:	3-31-91 (TW)
 */

print_other()
{

	move_csr(2,2);
	hi_lite("Select a file?   Use: Arrow Keys to move, Return to select, 'q' to abort"); 
	move_csr(24,3);
	hi_lite("n - next page, p - previous page");
	move_csr(38,20);
	hi_lite("h,j,k,l keys also move selection bar.");
}

/*	
 *	int read_printers(printers)
 *	
 *	Parameters:	printers - A list of printers read from file.
 *	
 *	Purpose:	Read printer info. from a file (PRINT_PATH).
 *
 *	Returns:	-1    = Failure.
 *			index = The number of printers defined.
 *
 *	Last Modify:	3-31-91 (TW)
 */

int read_printers(printers)
printer printers[MAX_PRINTERS];
{
	FILE *fp;
	int index = 0;
	char buff[256];
	
	if((fp = fopen(PRINT_PATH,"r")) == (FILE *)NULL){
	   return(-1);
	}else{
	   while(!feof(fp)){
	     Fgets(buff,256,fp);
	     if(buff[strlen(buff)-1] == '\n')
	       buff[strlen(buff)-1] = '\0';
	     if(strlen(buff) > 31)
	       buff[31] = '\0';
	     if(!feof(fp) && (index < MAX_PRINTERS)){
	        switch(buff[0]){
		   case '?':		/* Text comment user sees */
			memccpy(printers[index].comment,buff+2,'\0',30);
		   break;
		   case '*':		/* Printing Commmand	  */
			memccpy(printers[index].command,buff+2,'\0',30);
			index++;
		   break;
		   default:
		   break;
		}
	     }
	}
	fclose(fp);
    }
    return(index);	/* Return Count */
}

/*
 *	show_printers(printers,num)
 *
 *	Parameters:	printers - A list of printers read from file.
 *			num      - The Number of printers read.
 *
 *	Purpose:	Print The Printer selections on the screen.
 *
 *	Returns:	None
 *
 *	Last Modify:	3-31-91 (TW);
 */

void show_printers(printers,num)
printer printers[MAX_PRINTERS];
int num;
{
	int i, row = 6;

	for(i=0;i<num;i++){
	  if(!isposodd(i))
	    move_csr(5,row);
	  else{
	    move_csr(45,row);
	    row++;
	  }
	  print_str(printers[i].comment);
	}
}

/*
 *	char *choose_print(string)
 *
 *	Parameters:	string - The file to be printed.
 *	
 *	Purpose:	Allow User to choose a printer.
 *
 *	Returns:	A pointer to the printer string.
 *
 *	Last Modify:	3-31-91 (TW)
 */

char *choose_print(string)
char *string;
{
	printer printers[MAX_PRINTERS];
	int i = 0, row = 0, col = 1, num,ans,max;
	char buffer[256];

	fflush(stdout);
	num = read_printers(printers);
	show_printers(printers,num);
	move_csr(5,2);
	hi_lite(PRT_MENU);
	move_csr(5,4);
	(void) sprintf(buffer,"Filename: %s",string);
	hi_lite(buffer);
	move_csr(5,6);
	hi_lite(printers[i].comment);
	do{
	  switch(ans = get_input()){
	     case 'j':			/* Move Hi-Lite Bar down	*/
	     case 'J':
	     case DOWN:
		  if(col == 1)
		    max = in_col_one(num);
		  else
		    max = in_col_two(num);
		  if(row+1 < max){
		    if(col == 1)
		      move_csr(5,row+6);
	  	    else
		      move_csr(45,row+6);
		    print_str(printers[i].comment);
		    i+=2;
	            row++;
		    if(col == 1)
		      move_csr(5,row+6);
		    else
		      move_csr(45,row+6);
		    hi_lite(printers[i].comment);
		  }
	     break;
	     case 'k':			/* Move Hi-Lite Bar up  	*/
	     case 'K':
	     case UP:
		  if(row >= 1){
		    if(col == 1)
		      move_csr(5,row+6);
		    else
		      move_csr(45,row+6);
		    print_str(printers[i].comment);
		    i-=2;
	  	    row--;
		    if(col == 1)
		      move_csr(5,row+6);
		    else
		      move_csr(45,row+6);
		    hi_lite(printers[i].comment);
		  }
	     break;
	     case 'l':			/* Move Hi-Lite Bar right	*/
	     case 'L':
	     case RIGHT:
	   	  if(col == 1 && ((i + 1) < num)){
		    move_csr(5,row+6);
		    print_str(printers[i].comment);
		    col++;
		    i++;
		    move_csr(45,row+6);
		    hi_lite(printers[i].comment);
	          }
	     break;
	     case 'h':			/* Move Hi-Lite Bar left	*/
	     case 'H':
	     case LEFT:
	   	  if(col > 1){
		    move_csr(45,row+6);
		    print_str(printers[i].comment);
		    col--;
		    i--;
		    move_csr(5,row+6);
		    hi_lite(printers[i].comment);
		  }
	     break;
	     case '\r':
	     case '\n':
	          return(printers[i].command);
	     break;
	     case 'q':
	     case 'Q':
	          return('\0');
	     break;
	  }
	}while(ans != 'q');
	return('\0');
}

/* Not Implemented Completely */

# ifdef OPTS

/*
 *	print_op(index,FLAG)
 *
 *	Parameters:	index - Selection index (which) parameter doing.
 *			FLAG  - Hi-lite or Not.
 *	
 *	Returns:	None.
 *	
 *	Purpose:	Selecting Print time Options.
 *	
 *	Last Modify:	3-31-91 (TW)
 */

void print_op(index,FLAG)
int index;
int FLAG;
{
	char buffer[256];

	move_csr(print_opts[index].x,print_opts[index].y);
	if(index == 3){
  	   if(print_opts[index].option == 1)
    	      (void) sprintf(buffer,print_opts[index].selection," no");
  	   else
    	      (void) sprintf(buffer,print_opts[index].selection,"yes");
  	   }else{
    	      (void) sprintf(buffer,print_opts[index].selection,
    	      print_opts[index].option);
  	   }
	   if(FLAG == 0)
  	     print_str(buffer);
	   else
  	     hi_lite(buffer);
}

/*
 *	get_options(pr_cmd,lpr_cmd)
 *	
 *	Parameters:	pr_cmd  - The PR command and options.
 *			lpr_cmd - The LPR command and options.
 *
 *	Returns:	None.
 *	
 *	Purpose:	Select Special Print Time options.
 *
 *	Last Modify:	3-31-91 (TW)
 */

void get_options(pr_cmd,lpr_cmd)
char *pr_cmd, *lpr_cmd;
{
	int index = 0;
	char buffer[256];
	int input;
	
	move_csr(5,18);
	hi_lite("Use Arrow Keys to move, Space or Return to change/select.");
	print_op(0,1);
	print_op(1,0);
	print_op(2,0);
	print_op(3,0);
	print_op(4,0);
	print_op(0,1);
	do{
	input = get_input();
	switch(input){
	   case RIGHT:
		if(index < 4){
		  print_op(index,0);
	          index++;
		  print_op(index,1);
	 	}
	   break;
	   case LEFT:
		if(index > 0){
		  print_op(index,0);
	          index--;
		  print_op(index,1);
		}
	   break;
	   case ' ':
	   case '\n':
	   case '\r':
		if(print_opts[index].option == 1){
		  print_op(index,0);
		  print_opts[index].option++;
		  print_op(index,1);
		}else{
		  print_op(index,0);
		  print_opts[index].option--;
		  print_op(index,1);
		}
		if(index == 0){
		  fflush(stdout);
		  fprintf(stderr,"Ok, Not printing\n");
		  exit(0);
		}else
		  if(index == 1){
		    if(print_opts[2].option == 2)
		       (void) strcat(pr_cmd,"d");
		    if(print_opts[3].option == 2)
		         (void) strcat(lpr_cmd," -p");
		    if(print_opts[4].option == 2)
			 (void) strcat(lpr_cmd," -#2");
		    return;
		}
	   break;
	}
	}while(2 > 1);
}

# endif OPTS

/*
 *	char *do_selection(dirs)
 *	
 *	Parameters:	Allow directories
 *	
 *	Purpose:	Control module for selecting a file.
 *
 *	Returns:	The Selected File Name.
 *
 *	Last Modify:	3-31-91 (TW)
 */

static dir_info *dp = NULL;
char *do_selection(dirs)
int dirs;
{
	char dir_entries[MAX_ENT][ENT_LEN];
	int start_ent = 0, num = 0, end = 0;
	char *strng, buff[256];

	if (!dp)
		dp = (dir_info *) malloc(sizeof(dir_info));
	for (num=0; num<MAX_ENT; num++)
		dir_entries[num][0] = 0;
	while (1) {
# ifndef HPUX
		(void) getwd(buff);
# else
		(void) getcwd(buff,BUF_SIZ);
# endif HPUX
		if((num = get_dir(dir_entries,buff,dirs)) == -1)
		  return('\0');
		end = print_dir(dir_entries,start_ent,num,45,dp);
		strng = select_item(dir_entries,num,start_ent,end,dp);
		if (!strchr(strng,'/'))
			return(strng);
		chdir(strng);
	} 
}

/*
 *	do_printers()
 *	
 *	Parameters:	None
 *	
 *	Purpose:	Control module for  printing a file
 *
 *	Returns:	None.
 *
 *	Last Modify:	3-31-91 (TW)
 */

void do_printers()
{
	char *strng1, *strng2;
	char buff[256];

	move_csr(68,STATUS_LINE);
	hi_lite("           ");
	strng1 = do_selection();
	if(strng1 != NULL){
	  strng2 = choose_print(strng1);
	  if(strng2 != NULL){
	     Sprintf(buff,"%s %s",strng2,strng1);
	     exec_cshell(buff,CONTINUE);	
	  }else{
	     clr_area(3,2,19,73);
	  }
	}
	move_csr(68,STATUS_LINE);
	hi_lite(CMD_LIN);
}
