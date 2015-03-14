/* swap all word pairs of a file */

/* to be used as follows:

# swap all byte pairs of the file Scan.Tab
# reverse all BITSETs (long words) of the file Pars.Tab

dd conv=swab < Scan.Tab > .Scan.Tab; mv .Scan.Tab Scan.Tab
dd conv=swab < Pars.Tab | convert > .Pars.Tab; mv .Pars.Tab Pars.Tab
*/

# include <stdio.h>

main () {
   short w1, w2, n;
   
   for (;;) {
      n = fread (& w1, 2, 1, stdin);
      n = fread (& w2, 2, 1, stdin);
      if (! n) break;
      fwrite (& w2, 2, 1, stdout);
      fwrite (& w1, 2, 1, stdout);
   }
   return 0;
}
