#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/file.h>
#include <pwd.h>


FILE *scoreFile;
#define SCOREfILE "/usr/X386/lib/scores/xbombscores"
#define NUMsCORES 10
#define NAMEsIZE 50
#define SCOREfORMAT "%5d %s\n"
#define MAXpERnAME 2

struct
{
    unsigned score;
    char name[NAMEsIZE];
} scores[NUMsCORES] = {{999,""}};

openScores()
{
    if ((scoreFile = fopen (SCOREfILE, "r+")) == NULL)
    {
        fprintf(stderr,"Error: can't open score file\n");
    }
}

readScores()
{
    int     i;

    if (scoreFile == NULL) return;

    rewind(scoreFile);

    for (i = 0; i < NUMsCORES; i++) {
        fscanf (scoreFile,SCOREfORMAT, &scores[i].score, scores[i].name);
    }
}


printScores()
{
    int     i;

    for (i = 0; i < NUMsCORES; i++) {
        printf (SCOREfORMAT, scores[i].score, scores[i].name);
    }
}

writeScores()
{
    int     i;

    if (scoreFile == NULL) return;

    rewind(scoreFile);		/* okay, so the file never gets shorter */
    for (i = 0; i < NUMsCORES; i++) {
        fprintf (scoreFile, SCOREfORMAT, scores[i].score, scores[i].name);
    }
}

newScore(score)
unsigned score;
{
    int     i,
            loser;
    unsigned    scoresPerName;
    char   *name;

/*    if (scoreFile != NULL)
	flock (scoreFile, LOCK_EX);  */
    readScores ();

    scoresPerName = 0;
    name = getpwuid (getuid ()) -> pw_name;
    for (i = 0; i < NUMsCORES; i++) {
	if (strncmp (scores[i].name, name, NAMEsIZE) == 0) {
	    scoresPerName += 1;
	    loser = i;
	}
    }

    if (scoresPerName < MAXpERnAME) {/* push off the last person */
	loser = NUMsCORES - 1;
    }
    if (score < scores[loser].score) {/* deserves a high score */
	for (i = loser; i > 0; i--) {
	    if (score >= scores[i - 1].score)
		break;
	    scores[i].score = scores[i - 1].score;
	    strncpy (scores[i].name, scores[i - 1].name, NAMEsIZE - 1);
	}
	scores[i].score = score;
	strncpy (scores[i].name, name, NAMEsIZE);
	writeScores ();
    }

/*    if (scoreFile != NULL)
	flock (scoreFile, LOCK_UN);  */
}

print_scores()
{
    int i;
    char s[80];

    for (i = 0; i < NUMsCORES; i++)
    {
        sprintf(s,"%5d %s",scores[i].score,scores[i].name);
        WriteScore(i,s);
    }
}
