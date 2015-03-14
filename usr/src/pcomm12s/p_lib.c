/*
 * Routines to manipulate the pcomm.param file.
 */

#include <stdio.h>
#include "param.h"

/*
 * Read the parameter structure from the pcomm.param file.  Returns a
 * pointer to a static area containing the PARAM structure.  All errors
 * are fatal.
 */

struct PARAM *
read_param(extra)
char *extra;
{
	FILE *fp, *my_fopen();
	int i, line, oops;
	char buf[200], *temp_token, *str, *str_dup(), *findfile();
	char message[80], *str_tok();
	static char *token[NUM_PARAM] = {"D_BAUD", "D_PARITY", "D_DBITS",
	"D_SBITS", "HOT", "ASCII_HOT", "D_DUPLEX", "FLOW", "CR_IN", "CR_OUT",
	"LOGFILE", "DUMPFILE", "STRIP", "PAUSE_CHAR", "CR_CHAR", "CTRL_CHAR",
	"ESC_CHAR", "BRK_CHAR", "ABORT", "C_DELAY", "R_DELAY", "LECHO",
	"EXPAND", "CR_DELAY", "PACE", "CR_UP", "LF_UP", "TIMER", "CR_DN",
	"LF_DN", "LD_PLUS", "LD_MINUS", "LD_AT", "LD_POUND", "MAC_1",
	"MAC_2", "MAC_3", "MAC_4", "MAC_5", "MAC_6", "MAC_7", "MAC_8",
	"MAC_9", "MAC_0"};
	static struct PARAM p;
	void error_win();

	if ((p.p_path = findfile(extra, "pcomm.param")) == NULL)
		error_win(1, "Support file \"pcomm.param\" is missing", "or no read permission");

	if (!(fp = my_fopen(p.p_path, "r"))) {
		sprintf(buf, "\"%s\" for read", p.p_path);
		error_win(1, "Can't open parameter file", buf);
	}

	oops = 0;
	line = 0;
	for (i=0; i<NUM_PARAM; i++) {
		line++;
		if (fgets(buf, 200, fp) == NULL) {
			sprintf(message, "is truncated at line %d", line);
			oops++;
			break;
		}
					/* parse the input line */
		if (!(temp_token = str_tok(buf, '='))) {
			sprintf(message, "is missing a token at line %d", line);
			oops++;
			break;
		}
		if (!(str = str_tok((char *) NULL, '\n'))) {
			sprintf(message, "is missing a parameter at line %d", line);
			oops++;
			break;
		}
					/* sanity checking */
		if (strcmp(temp_token, token[i])) {
			sprintf(message, "is corrupted at line %d", line);
			oops++;
			break;
		}

		switch (i) {
					/* used in ls_menu() */
			case LINE_SET:
				p.d_baud = atoi(str);
				break;
			case LINE_SET+1:
				p.d_parity = *str;
				break;
			case LINE_SET+2:
				p.d_dbits = atoi(str);
				break;
			case LINE_SET+3:
				p.d_sbits = atoi(str);
				break;

					/* used in term_setup() */
			case TERM_SETUP:
				p.hot = atoi(str);
				break;
			case TERM_SETUP+1:
				p.ascii_hot = str_dup(str);
				break;
			case TERM_SETUP+2:
				p.d_duplex = str_dup(str);
				break;
			case TERM_SETUP+3:
				p.flow = str_dup(str);
				break;
			case TERM_SETUP+4:
				p.cr_in = str_dup(str);
				break;
			case TERM_SETUP+5:
				p.cr_out = str_dup(str);
				break;

					/* used in gen_setup() */
			case GEN_SETUP:
				p.logfile = str_dup(str);
				break;
			case GEN_SETUP+1:
				p.dumpfile = str_dup(str);
				break;
			case GEN_SETUP+2:
				p.strip = str_dup(str);
				break;
			case GEN_SETUP+3:
				p.pause_char = *str;
				break;
			case GEN_SETUP+4:
				p.cr_char = *str;
				break;
			case GEN_SETUP+5:
				p.ctrl_char = *str;
				break;
			case GEN_SETUP+6:
				p.esc_char = *str;
				break;
			case GEN_SETUP+7:
				p.brk_char = *str;
				break;
			case GEN_SETUP+8:
				p.abort = str_dup(str);
				break;

					/* used in gen_setup() delay_times() */
			case DELAY_TIMES:
				p.c_delay = atoi(str);
				break;
			case DELAY_TIMES+1:
				p.r_delay = atoi(str);
				break;

					/* used in axfer_setup() */
			case ASCII_SETUP:
				p.lecho = str_dup(str);
				break;
			case ASCII_SETUP+1:
				p.expand = str_dup(str);
				break;
			case ASCII_SETUP+2:
				p.cr_delay = atoi(str);
				break;
			case ASCII_SETUP+3:
				p.pace = str_dup(str);
				break;
			case ASCII_SETUP+4:
				p.cr_up = str_dup(str);
				break;
			case ASCII_SETUP+5:
				p.lf_up = str_dup(str);
				break;
			case ASCII_SETUP+6:
				p.timer = atoi(str);
				break;
			case ASCII_SETUP+7:
				p.cr_dn = str_dup(str);
				break;
			case ASCII_SETUP+8:
				p.lf_dn = str_dup(str);
				break;

					/* used in d_revise() */
			case LD_CODES:
				p.ld_plus = str_dup(str);
				break;
			case LD_CODES+1:
				p.ld_minus = str_dup(str);
				break;
			case LD_CODES+2:
				p.ld_at = str_dup(str);
				break;
			case LD_CODES+3:
				p.ld_pound = str_dup(str);
				break;

					/* used in macro() */
			case MACROS:
				p.mac_1 = str_dup(str);
				break;
			case MACROS+1:
				p.mac_2 = str_dup(str);
				break;
			case MACROS+2:
				p.mac_3 = str_dup(str);
				break;
			case MACROS+3:
				p.mac_4 = str_dup(str);
				break;
			case MACROS+4:
				p.mac_5 = str_dup(str);
				break;
			case MACROS+5:
				p.mac_6 = str_dup(str);
				break;
			case MACROS+6:
				p.mac_7 = str_dup(str);
				break;
			case MACROS+7:
				p.mac_8 = str_dup(str);
				break;
			case MACROS+8:
				p.mac_9 = str_dup(str);
				break;
			case MACROS+9:
				p.mac_0 = str_dup(str);
				break;
		}
	}
	fclose(fp);
	if (oops) {
		sprintf(buf, "Parameter file \"%s\"", p.p_path);
		error_win(1, buf, message);
	}
	return(&p);
}

/*
 * Write the updated param structure to disk.  The values in memory should
 * have already been "purified".  Later, we'll update only the entries that
 * have been explicitly asked for.  A non-zero return code means non-fatal
 * error.
 */

int
up_param()
{
	FILE *fp, *my_fopen();
	char buf[80];
	void error_win();
					/* open for write */
	if (!(fp = my_fopen(param->p_path, "w"))) {
		sprintf(buf, "\"%s\"", param->p_path);
		error_win(0, "No write permission on parameter file", buf);
		return(1);
	}

	fprintf(fp, "D_BAUD=%d\n", param->d_baud);
	fprintf(fp, "D_PARITY=%c\n", param->d_parity);
	fprintf(fp, "D_DBITS=%d\n", param->d_dbits);
	fprintf(fp, "D_SBITS=%d\n", param->d_sbits);
	fprintf(fp, "HOT=%d\n", param->hot);
	fprintf(fp, "ASCII_HOT=%s\n", param->ascii_hot);
	fprintf(fp, "D_DUPLEX=%s\n", param->d_duplex);
	fprintf(fp, "FLOW=%s\n", param->flow);
	fprintf(fp, "CR_IN=%s\n", param->cr_in);
	fprintf(fp, "CR_OUT=%s\n", param->cr_out);
	fprintf(fp, "LOGFILE=%s\n", param->logfile);
	fprintf(fp, "DUMPFILE=%s\n", param->dumpfile);
	fprintf(fp, "STRIP=%s\n", param->strip);
	fprintf(fp, "PAUSE_CHAR=%c\n", param->pause_char);
	fprintf(fp, "CR_CHAR=%c\n", param->cr_char);
	fprintf(fp, "CTRL_CHAR=%c\n", param->ctrl_char);
	fprintf(fp, "ESC_CHAR=%c\n", param->esc_char);
	fprintf(fp, "BRK_CHAR=%c\n", param->brk_char);
	fprintf(fp, "ABORT=%s\n", param->abort);
	fprintf(fp, "C_DELAY=%d\n", param->c_delay);
	fprintf(fp, "R_DELAY=%d\n", param->r_delay);
	fprintf(fp, "LECHO=%s\n", param->lecho);
	fprintf(fp, "EXPAND=%s\n", param->expand);
	fprintf(fp, "CR_DELAY=%d\n", param->cr_delay);
	fprintf(fp, "PACE=%s\n", param->pace);
	fprintf(fp, "CR_UP=%s\n", param->cr_up);
	fprintf(fp, "LF_UP=%s\n", param->lf_up);
	fprintf(fp, "TIMER=%d\n", param->timer);
	fprintf(fp, "CR_DN=%s\n", param->cr_dn);
	fprintf(fp, "LF_DN=%s\n", param->lf_dn);
	fprintf(fp, "LD_PLUS=%s\n", param->ld_plus);
	fprintf(fp, "LD_MINUS=%s\n", param->ld_minus);
	fprintf(fp, "LD_AT=%s\n", param->ld_at);
	fprintf(fp, "LD_POUND=%s\n", param->ld_pound);
	fprintf(fp, "MAC_1=%s\n", param->mac_1);
	fprintf(fp, "MAC_2=%s\n", param->mac_2);
	fprintf(fp, "MAC_3=%s\n", param->mac_3);
	fprintf(fp, "MAC_4=%s\n", param->mac_4);
	fprintf(fp, "MAC_5=%s\n", param->mac_5);
	fprintf(fp, "MAC_6=%s\n", param->mac_6);
	fprintf(fp, "MAC_7=%s\n", param->mac_7);
	fprintf(fp, "MAC_8=%s\n", param->mac_8);
	fprintf(fp, "MAC_9=%s\n", param->mac_9);
	fprintf(fp, "MAC_0=%s\n", param->mac_0);

	fclose(fp);
	return(0);
}
