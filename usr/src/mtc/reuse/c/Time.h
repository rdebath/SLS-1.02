# ifndef yyTime
# define yyTime

# ifdef __STDC__
# define ARGS(parameters)	parameters
# else
# define ARGS(parameters)	()
# endif

extern int	StepTime	();
			/* Returns the sum of user time and system time */
			/* since the last call to 'StepTime' in milli-	*/
			/* seconds.					*/
 
extern void	WriteStepTime	ARGS((char * string));
			/* Writes a line consisting of the string	*/
			/* 'string' and the value obtained from a call	*/
			/* to 'StepTime' on standard output.		*/

# endif
