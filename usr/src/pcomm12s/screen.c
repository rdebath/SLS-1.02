/*
 * Routines to read and copy the virtual screen image file.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "param.h"
#include "status.h"

/*
 * Do a screen dump.  Actually, the screen is already dumped, all we
 * do is copy the file.
 */

void
screen_dump()
{
	FILE *fp_in, *fp_out, *my_fopen();
	char buf[MAX_COL];
	void error_win();
	int i;
					/* open for append */
	if (!(fp_out = my_fopen(param->dumpfile, "a"))) {
		sprintf(buf, "\"%s\" for write", param->dumpfile);
		error_win(0, "Can't open screen dump file", buf);
		return;
	}
#ifdef SHAREDMEM
	for (i=0; i<LINES; i++)
		fprintf(fp_out, "%s\n", status->vs[i]);

#else /* SHAREDMEM */
					/* not guaranteed to exist yet */
	if (!(fp_in = my_fopen(status->vs_path, "r"))) {
		fclose(fp_in);
		return;
	}
					/* skip the x, y coordinates */
	fgets(buf, 10, fp_in);

	while (fgets(buf, MAX_COL, fp_in) != NULL)
		fputs(buf, fp_out);

	fclose(fp_in);
#endif /* SHAREDMEM */

	fclose(fp_out);

	return;
}

/*
 * Read the virtual screen and paint its contents to the stdscr using
 * curses(3).  Move the cursor where it belongs.
 */

void
load_vs()
{
	register int i;
	FILE *fp, *my_fopen();
	int row, col, max_col;
	char buf[MAX_COL];

	clearok(curscr, TRUE);
	erase();

#ifdef SHAREDMEM
	for (i=0; i<LINES; i++)
		mvaddstr(i, 0, status->vs[i]);

	move(status->row, status->col);
#else /* SHAREDMEM */
					/* not guaranteed to exist yet */
	if (!(fp = my_fopen(status->vs_path, "r")))
		return;
					/* get the x, y coordinates */
	fgets(buf, 10, fp);
	sscanf(buf, "%d,%d\n", &row, &col);

	i = 0;
	max_col = (COLS > MAX_COL-1) ? MAX_COL-1 : COLS;
	while (fgets(buf, MAX_COL, fp) != NULL) {
					/* zap the line feed */
		buf[max_col] = '\0';
		mvaddstr(i++, 0, buf);
	}
	fclose(fp);
	move(row, col);
#endif /* SHAREDMEM */

	refresh();
	return;
}

/*
 * Zap the virtual screen file (or clear it).
 */

void
zap_vs()
{
#ifdef SHAREDMEM
	status->clr = 1;
#else /* SHAREDMEM */
	unlink(status->vs_path);
#endif /* SHAREDMEM */
	return;
}
