
# include <stdio.h>
# include <ctype.h>
# include <math.h>

# define TRUE 1
# define FALSE 0

read_input(string)
char *string;
{				/* Read input into an array */
	while((*string = getchar()) != '\n'){
	   *string++;
	}
	*string = '\0';
}

read_str1(string)	/* Read a string if you are not in raw mode */
char *string;
{
	char *str;

	no_echo();
	cbreak();
	str = string;
	while((*str = getchar()) != '\n'){
	   if(*str ==  0x08  || *str == 0x7f)
	      if(*str != *string){
		 outc(0x08);
		 outc(0x20);
		 outc(0x08);
		 *str--;
	      }else
		 continue;
	   else{
	      outc(*str);
	      *str++;
	   }
	}
	*str = '\0';
	echo();
	cbreak();
}

chop_str(string)
char *string;
{
	int length;
	
	length = strlen(string);
	while(*string != '\0')
	   *string++;
	for(i=length;i>70;i--,*string--)
	  *string = '\0';
}

char *str_pre_blank(string)
char *string;
{				/* Strip off preceding Blanks */
	while(*string == ' ')
	   *string++;
	return(string);
}

str_trl_blank(string)
char *string;			/* Strip Blanks from end of line */
{
	while(!isspace(*string))
	   *string++;
	*string = '\0';
}

int is_integer(string)
char *string;			/* Is the string an integer?  */
{
	if((strlen(string)) == 0)
	  return(FALSE);
	if(*string == '-')
	  *string++;
	for(;*string != '\0'; *string++)
	    if(!isdigit(*string))
	       return(FALSE);
	return(TRUE);
}

int is_float(string)
char *string;			/* Is the string a floating point number? */
{				/* Invalid: (.),(.0),(1.0.) any string    */
	int flag = FALSE;	/* containing a non-digit.		  */

	if((strlen(string)) == 0)
	   return(FALSE);
	if(!isdigit(*string) && *string != '-')
	   return(FALSE);
	else
	   *string++;
	for(;*string != '\0'; *string++){
	   if(!isdigit(*string) && *string != '.')
	     return(FALSE);
	   if(*string == '.' && flag == TRUE)
	     return(FALSE);
	   if(*string == '.')
	      flag = TRUE;
	}
	return(TRUE);
}

