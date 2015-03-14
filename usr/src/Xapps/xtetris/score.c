#include "defs.h"

#define HIGH_TABLE_SIZE 10      /* size of high score table */
static struct score_table {
	char    name[80];
	int     score;
	int     rows;
	int     level;
	char hostname[80];
	char    date[80];
}       high_scores[HIGH_TABLE_SIZE];

update_highscore_table()
{
  /* This version only allows 1 entry in the HIGH SCORE TABLE per user */
  int     i, j;
  long    when;
  extern char *ctime();
  extern long time();
  char    hostname[BUFSIZ];
  char    buf[BUFSIZ];
  char	  padname[25];
  
  strcpy( padname, "                    ");
  strncpy( padname, name, strlen(name) );	/* get padded name. */

  if (!resources.usescorefile) return;
  
  /*	re-read high-score table in case someone else on the network is
   *	playing at the same time 
   */
  read_high_scores();

  /* Check for previous best score */
  for (i = 0; i < HIGH_TABLE_SIZE; i++)
  {
    if (strcmp(padname, high_scores[i].name) == 0)
      break;
  }
  if (i < HIGH_TABLE_SIZE) {
    /*
     *	We have a previous best score. 
     */
    if (high_scores[i].score >= score)
      return;         /* Same/worse score - no update */
    for (j = i; j > 0; j--) /* Remove previous best */
      high_scores[j] = high_scores[j - 1];
  }
  /* Next line finds score greater than current one */
  for (i = 0;
       i < HIGH_TABLE_SIZE && score >= high_scores[i].score; 
       i++);
  i--;
  if (i >= 0) {
    for (j = 0; j < i; j++)
      high_scores[j] = high_scores[j + 1];
    strcpy(high_scores[i].name, name);
    high_scores[i].score = score;
    high_scores[i].rows = rows;
    high_scores[i].level = rows / 10;
    if (_XGetHostname(hostname, BUFSIZ) == -1)
      strcpy(high_scores[i].hostname, "unknown-host");
    else
      strcpy(high_scores[i].hostname, hostname);
    time(&when);
    strncpy(high_scores[i].date, ctime(&when), 24); 
    high_scores[i].date[24] = 0;
    write_high_scores();
  }
}


read_high_scores()
{
  FILE   *fp;
  int     i;
  char   buf[BUFSIZ];
  
  if (!resources.usescorefile) return;
  
  if ((fp = fopen(resources.scorefile, "r")) == NULL) {
    write_high_scores();
    if ((fp = fopen(resources.scorefile, "r")) == NULL) {
      resources.usescorefile = False;
      fprintf( stderr, "%s: No High score file.  Use '-noscore' to avoid this message.\n",
	      programname );
      return;
    }
  }
  for (i = 0; i < HIGH_TABLE_SIZE; i++) {
    struct score_table *score = &(high_scores[i]);
    if (6 != fscanf( fp, "%20[^,],%7d,%5d,%4d,%12[^,],%24[^,]\n",
		    score->name, 
		    &score->score, 
		    &score->level,
		    &score->rows, 
		    score->hostname, 
		    score->date ))
    {
      strcpy( score->name, "No name" );
      strcpy( score->hostname, "No host" );
      strcpy( score->date, "No date" );
      score->rows = score->score = score->level = 0;
    }
  }
  fclose(fp);
}

write_high_scores()
{
  FILE   *fp;
  int     i;
  
  if ((fp = fopen(resources.scorefile, "w")) == NULL) {
    fprintf(stderr, "%s: Couldn't open high score file %s\n", programname, resources.scorefile );
    return;
  }
  for (i = 0; i < HIGH_TABLE_SIZE; i++)
    fprintf( fp, "%-20s,%7d,%5d,%4d,%-12s,%-24s\n",
	   high_scores[i].name, 
	   high_scores[i].score, 
	   high_scores[i].level,
	   high_scores[i].rows, 
	   high_scores[i].hostname, 
	   high_scores[i].date );
  fclose(fp);
}

void print_high_scores()
{
  int     i,j;
static  char    buf[81*HIGH_TABLE_SIZE+100];
  char * start;
  int len;
  
  if (!resources.usescorefile) return;
  
  /* re-read high-score table in case someone else on the network is
   * playing at the same time */

  read_high_scores();

  sprintf( buf, "\
Name                 Score   Level Rows Host         Date\n\
----                 -----   ----- ---- ----         ----\n" );
  
  len = strlen(buf);
  start = buf+len;
  
  for (i = HIGH_TABLE_SIZE-1; i >= 0; i--)
  {
    if (high_scores[i].score != 0)
    {
      int chars;
      sprintf( start, "%-20s %-7d %-5d %-4d %-12s %-24s\n",
	      high_scores[i].name, 
	      high_scores[i].score, 
	      high_scores[i].level,
	      high_scores[i].rows, 
	      high_scores[i].hostname, 
	      high_scores[i].date );
      chars = strlen(start);
      start += chars;
      len += chars;
    }
  }
  XtVaSetValues( score_text, 
		XtNstring, (XtArgVal)buf,
		XtNlength, (XtArgVal)len+1, NULL );
  XtPopupSpringLoaded(score_frame);
}

/*
  emacs mode: indented-text
  
  emacs Local Variables: 
  emacs mode: c 
  emacs c-indent-level: 2
  emacs c-continued-statement-offset: 2
  emacs c-continued-brace-offset: -2
  emacs c-tab-always-indent: nil
  emacs c-brace-offset: 0 
  emacs tab-width: 8
  emacs tab-stop-list: (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84)
  emacs End:
  */
