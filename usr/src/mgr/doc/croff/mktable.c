/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: mktable.c,v 1.1 88/07/07 10:12:17 sau Exp $
	$Source: /tmp/mgrsrc/doc/usrman/croff/RCS/mktable.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/doc/usrman/croff/RCS/mktable.c,v $$Revision: 1.1 $";

/* make a compile time hash table  (SAU)
 *
 * usage: mktable <name> <size>
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include "hash.h"

#define SIZE		71
#define MIN_SIZE	5

char key[80];
char value[80];

main(argc,argv)
int argc;
char **argv;
   {
   register int i,code, count = 0;
   TABLE **table;			/* place to build hash table into */
   register TABLE *list, *next;
   char *name;				/* name of table */
   char *prog = *argv;
   int len=0, size;
   char tmp[80];

   int r_flag = 0;			/* reverse sense of keyword-value */
   int n_flag = 0;			/* no values */

   for(;argc>1 && *argv[1]=='-';argv++,argc--)
      switch(argv[1][1]) {
         case 'r': r_flag++; break;
         case 'n': case '1': n_flag++; *value='\0'; break;
         default : fprintf(stderr,"%s: flag %s ignored\n",prog,argv[1]);
         }

   if (argc < 3) {
      fprintf(stderr,"usage: %s [-r -n] <name> <number of buckets>\n",prog);
      exit(1);
      }

   name = argv[1];

   if ((size = atoi(argv[2])) < MIN_SIZE)
      size = SIZE;

   if ((table = (TABLE **) malloc(size * sizeof(TABLE))) == NULL) {
      perror("Can't alloc space for table");
      exit(1);
      }

   bzero(table,size*sizeof(TABLE));

   /* build the hash table */

   while(1) {
      if (n_flag)
         code = scanf("%s\n",key);
      else if (r_flag)
         code = scanf("%s %s\n",value,key);
      else
         code = scanf("%s %s\n",key,value);

      if (code == EOF)
         break;
      add_entry(table,size,key);
      put_entry(table,size,key,value);
      count++;
      }

   /* print out data */

   printf("/* hash table: %d items in %d buckets */\n\n",count,size);
   printf("#include <stdio.h>\n");
   printf("#include \"hash.h\"\n\n");
   printf("#define SIZE_%s	%d\n\n",name,size);
   printf("struct table_entry %s_data[] = {\n",name);

   for(count=0,i=0;i<size;i++) {
      for(code=0,list=table[i];list != (TABLE *) 0; list = next,count++) {
         next = list->next;
         printf("   {\"%s\", \"%s\", %d, 0x%x, %s},",
               list->name, list->value, list->count, HASH_STATIC,
               next ? sprintf(tmp,"&%s_data[%d]",name,count+1),tmp : "NULL");
         if (code++ == 0)
            printf("	/*  %d */\n",i);
         else
            printf("\n");
         }
      }
   printf("   };\n\n");

   /* print out hash table */

   printf("/* hash table: %d items */\n\n",size);
   printf("struct table_entry *%s[] = {\n  ",name);

   for(len=2,code=count=i=0;i<size;i++) {
      for(list=table[i]; list != (TABLE *) 0; list = list->next) {
         count++;
         code++;
         }
      if (code) {
         sprintf(tmp," &%s_data[%d],",name,count-code);
         code = 0;
         }
      else
         sprintf(tmp," NULL,");

      len += strlen(tmp);

      if (len > 78) {
         printf("\n  ");
         len = 2 + strlen(tmp);
         }

      printf("%s",tmp);
      }
   if (len)
      printf("\n  ");
   printf(" };\n\n");
   exit(0);
   }
