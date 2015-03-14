/* testlib.c - test expectlib */

#include <stdio.h>
#include "expect.h"

timedout()
{
	fprintf(stderr,"timed out\n");
	exit(-1);
}

char move[100];

read_first_move(fp)
FILE *fp;
{
	if (EXP_TIMEOUT == exp_fexpectl(fp,"first\r\n1.*\r\n",0,(char *)0)) {
		timedout();
	}
	sscanf(exp_match,"%*s 1. %s",move);
}

/* moves and counter-moves are printed out in different formats, sigh... */

read_counter_move(fp)
FILE *fp;
{
	switch (exp_fexpectl(fp,"*...*\r\n",0,(char *)0)) {
	case EXP_TIMEOUT: timedout();
	case EXP_EOF: exit(-1);
	}

	sscanf(exp_match,"%*s %*s %*s %*s ... %s",move);
}

read_move(fp)
FILE *fp;
{
	switch (exp_fexpectl(fp,"*...*\r\n*.*\r\n",0,(char *)0)) {
	case EXP_TIMEOUT: timedout();
	case EXP_EOF: exit(-1);
	}

	sscanf(exp_match,"%*s %*s ... %*s %*s %s",move);
}

send_move(fp)
FILE *fp;
{
	fprintf(fp,move);
}

main(){
	FILE *fp1, *fp2;
	int ec;

/*	exp_is_debugging = 1;*/
	exp_loguser = 1;
	exp_timeout = 3600;

	if (0 == (fp1 = exp_popen("chess"))) {
		printf("exp_popen failed\n");
		exit();
	}

	if (0 > exp_fexpectl(fp1,"Chess\r\n",0,(char *)0)) exit();
	fprintf(fp1,"first\r");

	read_first_move(fp1);

	fp2 = exp_popen("chess");

	exp_fexpectl(fp2,"Chess\r\n",0,(char *)0);

	for (;;) {
		send_move(fp2);
		read_counter_move(fp2);

		send_move(fp1);
		read_move(fp1);
	}
}
