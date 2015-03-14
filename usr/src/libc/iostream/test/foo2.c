/*
Hello all.

The guy who wrote cim wrote to me, he had bad conscience for pushing
back characters that were diferent from what he got. And indeed that
seems to be the problem. I wrote this little thingy:


------ungettest.c------
*/
#include <stdio.h>

int main(void)

{
  FILE *input;
  int c;
  int err;

  input = fopen("/tmp/foo","r");
  c=getc(input);
  c=getc(input);
  err=ungetc(c,input);   /* Here we put the same as we got back */
  printf("Correct ftell: %d err: %d\n",ftell(input),err);
  c=getc(input);
  printf("Got: %d=='%c' ftell now: %d\n",c,c,ftell(input));
  fclose(input);

  input = fopen("/tmp/foo","r");
  c=getc(input);
  c=getc(input);
  err=ungetc('A',input); /* Here we put something else back */
  printf("Wrong ftell: %d err: %d\n",ftell(input),err);
  printf("Got: %d=='%c' ftell now: %d\n",c,c,ftell(input));
  fclose(input);

  /* The man pages I have read does not say that you have to push back
     the same char as the one you got previously. On my linux box this
     produces this output (gcc 2.3.3 4.2, libs)

       Correct ftell: 1 err: 105
       Got: 105=='i' ftell now: 2
       Wrong ftell: -1 err: 65
       Got: 105=='i' ftell now: -1

     On a univ. sparc this is the output:

       Correct ftell: 1 err: 105
       Got: 105=='i' ftell now: 2
       Wrong ftell: 1 err: 65
       Got: 105=='i' ftell now: 1

     Oops, a bug in the test prog., :(, still -1 is a highly bogus ftell.

  */
}
