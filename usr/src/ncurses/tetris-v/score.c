#include "defs.h"
#include <ncurses.h>
#include <sys/utsname.h>

#define HIGH_SCORE_TABLE	"/usr/games/lib/Tetris_scores"

#define HIGH_TABLE_SIZE	10

static struct score_table {
        char    name[BUFSIZ];
        int     score;
        int     rows;
        int     level;
	char	hostname[BUFSIZ];
        char    date[BUFSIZ];
} high_scores[HIGH_TABLE_SIZE];

update_highscore_table()
{
        int     i, j;
        long    when;
        extern char *ctime();
        extern long time();
        char    buf[BUFSIZ];
	struct utsname utsname;

        /* re-read high-score table in case someone else on the network is
         * playing at the same time */
        read_high_scores();

        /* Next line finds score greater than current one */
        for (i = 0; ((i < HIGH_TABLE_SIZE) && (score >= high_scores[i].score)); i++);
        i--;
        score_position = i;
        if (i >= 0) {
                for (j = 0; j < i; j++)
                        high_scores[j] = high_scores[j + 1];
                strcpy(high_scores[i].name, name);
                high_scores[i].score = score;
                high_scores[i].rows = rows;
                high_scores[i].level = rows / 10;
		if ( uname(&utsname) < 0 )
                        strcpy(high_scores[i].hostname, "unknown-host");
                else
                        strcpy(high_scores[i].hostname, utsname.nodename);
                time(&when);
                strcpy(buf, ctime(&when));      /* ctime() adds a newline
                                                 * char */
                strip_eoln(buf);/* so remove it          */
                strcpy(high_scores[i].date, buf);
                write_high_scores();
        }
}

read_high_scores()
{
        FILE   *fp;
        int     i;
        char   buf[BUFSIZ];

        for (i = 0; i < HIGH_TABLE_SIZE; i++) {
                strcpy(high_scores[i].name, " ");
                high_scores[i].score = 0;
                high_scores[i].rows = 0;
                high_scores[i].level = 0;
                strcpy(high_scores[i].hostname, " ");
                strcpy(high_scores[i].date, " ");
        }
        if ((fp = fopen(HIGH_SCORE_TABLE, "r")) == NULL) {
                fprintf(stderr, "tetris: No High score file\n");
                return;
        }
        for (i = 0; i < HIGH_TABLE_SIZE; i++) {
                fgets(buf, BUFSIZ, fp);
                strip_eoln(buf);
                strcpy(high_scores[i].name, buf);
                fgets(buf, BUFSIZ, fp);
                strip_eoln(buf);
                high_scores[i].score = atoi(buf);
                fgets(buf, BUFSIZ, fp);
                strip_eoln(buf);
                high_scores[i].rows = atoi(buf);
                fgets(buf, BUFSIZ, fp);
                strip_eoln(buf);
                high_scores[i].level = atoi(buf);
                fgets(buf, BUFSIZ, fp);
                strip_eoln(buf);
                strcpy(high_scores[i].hostname, buf);
                fgets(buf, BUFSIZ, fp);
                strip_eoln(buf);
                strcpy(high_scores[i].date, buf);
        }
        fclose(fp);
}

strip_eoln(s)
        char   *s;
{
        char   *s1;

        while (*s != '\0') {
                if (*s == '\n') {       /* End of line char */
                        s1 = s;
                        do {
                                *s1 = *(s1 + 1);        /* Copy rest of string */
                                s1++;
                        } while (*s1 != '\0');
                } else
                        s++;
        }
}

write_high_scores()
{
        FILE   *fp;
        int     i;

        if ((fp = fopen(HIGH_SCORE_TABLE, "w")) == NULL) {
                fprintf(stderr, "tetris: Couldn't open high score file %s\n", HIGH_SCORE_TABLE);
                return;
        }
        for (i = 0; i < HIGH_TABLE_SIZE; i++)
                fprintf(fp, "%s\n%d\n%d\n%d\n%s\n%s\n",
                        high_scores[i].name,
                        high_scores[i].score,
                        high_scores[i].rows,
                        high_scores[i].level,
                        high_scores[i].hostname,
                        high_scores[i].date);
        fclose(fp);
}

void print_high_scores()
{
        int     i;
        char    buf[BUFSIZ];

        /* re-read high-score table in case someone else on the network is
         * playing at the same time */
        read_high_scores();

	clear();
	wtext(10,1,"T e t r i s   H i g h e s t   r e s u l t s", 0);

	wtext(4,3,"Pos  Name             Score  Rows Lev  When", 0);
	wtext(4,4,"===  ====             =====  ==== ===  ====", 0);
	for ( i = 0; i < HIGH_TABLE_SIZE; i++ ) {
                sprintf(buf, "%3d) %-15s %6d %5d %3d  %s\n",
                        HIGH_TABLE_SIZE - i,
                        high_scores[i].name,
                        high_scores[i].score,
                        high_scores[i].rows,
                        high_scores[i].level,
                        high_scores[i].date);
		wtext(4,14-i,buf, score_position == i);
        }
}

print_authors()
{
	int i;
	static char *au[] = {
"\n",
"    Tetris Version 1.0\n\n",
"This version of tetris was modified to run on ascii terminals by:\n",
"    Roberto Biancardi     <..!unido!tmpmbx!deejay!i2ack!usixth!bob>\n",
"Based on the version posted by Phill Everson <everson@cs.bris.ac.uk>\n",
"and Martyn Shortley <shortley@cs.bris.ac.uk>, based on the version posted\n",
"to comp.sources.games by Adam Marguilies <vespa@ssyx.ucsc.edu>\n",
"\n",
"Wed Jun 21 22:35:52 ITA 1989\n",
NULL
};

	for ( i=0; au[i] != NULL; i++ )
		addstr(au[i]);
	getch(); 
}
