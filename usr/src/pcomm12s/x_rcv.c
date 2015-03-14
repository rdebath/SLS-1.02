/*
 * Receive a list of files using a version of Ward Christensen's file
 * transfer protocol.  A non-zero return code means the user must acknowledge
 * an error condition (a user generated abort returns a 0).  Write errors
 * are considered fatal.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "xmodem.h"

unsigned char buf[1029];
char file_name[15];
long file_length;

static int err_method, tot_err, block_size;

int
rcv_xmodem(win, list, type, fast)
WINDOW *win;
char *list;
int type, fast;
{
	extern char *protocol[];
	FILE *fp, *my_fopen();
	int i, default_err, is_batch, max_block, code, file_count, got_hdr;
	int hours, mins, secs, len;
	long block, recv, partial;
	float percent, performance;
	unsigned char blk;
	unsigned int sleep();
	char *file, *name, *strcpy(), *strrchr(), *strtok();
	void cancel_xfer();
					/* which protocol? */
	switch (type) {
		case XMODEM:
			default_err = CRC_CHECKSUM;
			is_batch = 0;
			max_block = 128;
			break;
		case XMODEM_1k:
			default_err = CRC_CHECKSUM;
			is_batch = 0;
			max_block = 1024;
			break;
		case MODEM7:
			default_err = CHECKSUM;
			is_batch = 1;
			max_block = 128;
			break;
		case YMODEM:
			default_err = CRC;
			is_batch = 1;
			max_block = 1024;
			performance = 1.09;
			break;
		case YMODEM_G:
			default_err = NONE;
			is_batch = 1;
			max_block = 1024;
			performance = 1.02;
			break;
		default:
			return(1);
	}

	tot_err = 0;
	file_count = 0;
	mvwaddstr(win, 2, 24, protocol[type]);
	mvwaddstr(win, 11, 24, "0  ");

	while (1) {
		file_count++;
		file_length = 0L;
					/* user supplied name */
		if (!is_batch) {
			if (file_count > 1)
				break;

			file = strtok(list, " \t");
					/* dissect the file name */
			if ((name = strrchr(file, '/')))
				strcpy(file_name, ++name);
			else
				strcpy(file_name, file);
		}
					/* get the modem7 file name */
		if (type == MODEM7) {
			if (code = rcv_modem7(win, default_err))
				return(code +1);

			file = file_name;
		}
					/* get the block 0 */
		if (type == YMODEM || type == YMODEM_G) {
			if (code = send_first(win, max_block, default_err))
				return(code +1);

			if (code = rcv_ymodem(win))
				return(code +1);

					/* at the end? */
			if (buf[3] == '\0') {
				beep();
				wrefresh(win);
				putc_line(ACK);
				sleep(1);
				return(0);
			}
			file = file_name;
		}
					/* any trouble? */
		if (file_name[0] == '\0')
			continue;

		clear_line(win, 3, 24, TRUE);
		waddstr(win, file_name);
					/* if file length is known */
		if (file_length != 0L) {
			mvwprintw(win, 4, 24, "%-10ld", file_length);

			secs = (file_length * 10.0 / dir->baud[dir->d_cur]) * performance;
			hours = secs / 3600;
			mins = (secs % 3600) / 60;
			secs = (secs % 3600) % 60;

			mvwprintw(win, 6, 24, "%d:%02d:%02d", hours, mins, secs);
		}
					/* some starting numbers */
		mvwaddstr(win, 7, 24, "0    ");
		if (file_length != 0L && fast)
			mvwaddstr(win, 8, 24, "0%  ");
		if (fast)
			mvwaddstr(win, 9, 24, "0          ");
		mvwaddstr(win, 10, 24, "0 ");
		clear_line(win, 12, 24, TRUE);
		waddstr(win, "NONE");
		wrefresh(win);

		/*
		 * If the user supplied the name, write permission is checked
		 * by the get_names() routine in xfer_menu().  If modem7
		 * or ymodem supplied name, the name is unique and the write
		 * permission on the directory is checked by the change_name()
		 * routines.  However, this is required for systems with
		 * SETUID_BROKE set.
		 */
					/* open the file */
		if (!(fp = my_fopen(file, "w"))) {
			beep();
			clear_line(win, 12, 24, TRUE);
			wattrstr(win, A_BOLD, "CAN'T OPEN FILE");
			wrefresh(win);
			cancel_xfer(DOWN_LOAD);
			return(1);
		}
					/* ACK the block 0 */
		if (type == YMODEM || type == YMODEM_G)
			putc_line(ACK);

		if (code = send_first(win, max_block, default_err)) {
			fclose(fp);
			return(code +1);
		}
					/* here we go... */
		clear_line(win, 12, 24, TRUE);
		waddstr(win, "NONE");
		wrefresh(win);
		blk = 1;
		block = 1L;
		recv = 0L;
		got_hdr = 1;
		while (1) {
			code = rcv_block(win, got_hdr, max_block, blk);

			if (code < 0) {
				fclose(fp);
				return(code +1);
			}
			got_hdr = 0;
					/* are we done? */
			if (buf[0] == EOT) {
				if (!is_batch) {
					beep();
					wrefresh(win);
					sleep(1);
				}
				break;
			}
					/* if not a duplicate block */
			if (!code) {
				if (file_length != 0L) {
					partial = file_length - recv;
					if (partial > (long) block_size)
						len = block_size;
					else
						len = partial;
				}
				else
					len = block_size;

				if (fwrite((char *) &buf[3], sizeof(char), len, fp) != len) {
					beep();
					clear_line(win, 12, 24, TRUE);
					wattrstr(win, A_BOLD, "WRITE ERROR");
					wrefresh(win);
					cancel_xfer(DOWN_LOAD);
					fclose(fp);
					/* fatal */
					return(1);
				}
				mvwprintw(win, 7, 24, "%-5ld", block);
				recv = recv + (unsigned int) len;
				if (fast)
					mvwprintw(win, 9, 24, "%-10ld", recv);
				blk++;
				block++;
			}
			/*
			 * If the length is known, give the same status
			 * report as uploading
			 */
			if (file_length != 0L && fast) {
				percent = recv * 100.0 / file_length;
				if (percent > 100.0)
					percent = 100.0;
				mvwprintw(win, 8, 24, "%0.1f%%", percent);
			}
			wrefresh(win);
			putc_line(ACK);
		}
		if (file_length != 0L && fast) {
			mvwaddstr(win, 8, 24, "100%  ");
			wrefresh(win);
		}
		/*
		 * If the file length is not known, search backwards from
		 * the end of the file until you find a character that is
		 * not the ^Z padding character.
		 */
		if (file_length == 0L) {
			for (i=block_size+2; i>2; i--) {
				if (buf[i] != CTRLZ)
					break;
			}
			file_length = recv - (unsigned int) block_size + (unsigned int) i -2L;
			fclose(fp);
			if (fix_length(file, file_length)) {
				beep();
				clear_line(win, 12, 24, TRUE);
				wattrstr(win, A_BOLD, "TRUNCATE ERROR");
				wrefresh(win);
				sleep(1);
			}
		}
		else
			fclose(fp);
					/* ACK the EOT */
		putc_line(ACK);
	}
	return(0);
}

/*
 * Send the first character to start the transmission and set the error
 * checking method.  Returns the standard error codes or 0 on success.
 * The variables err_method and block_size are global.
 */

static int
send_first(win, max_block, default_err)
WINDOW *win;
int max_block, default_err;
{
	int i, err_count;
	unsigned int sleep();
	void cancel_xfer();
					/* default error method */
	err_method = default_err;
	if (default_err == CRC_CHECKSUM)
		err_method = CRC;
					/* send the first char */
	err_count = 0;
	while (err_count < MAX_ERRORS*2) {
		mvwprintw(win, 10, 24, "%-2d", err_count);

					/* check for keyboard abort */
		if (wgetch(win) == ESC) {
			beep();
			clear_line(win, 12, 24, TRUE);
			waddstr(win, "ABORTED");
			wrefresh(win);
			cancel_xfer(DOWN_LOAD);
			sleep(3);
			return(ABORT);
		}
					/* switch to checksum? */
		if (default_err == CRC_CHECKSUM && err_count > MAX_ERRORS/2)
			err_method = CHECKSUM;

					/* send error method code */
		clear_line(win, 5, 24, TRUE);
		switch (err_method) {
			case CHECKSUM:
				waddstr(win, "CHECKSUM");
				putc_line(NAK);
				break;
			case CRC:
				waddstr(win, "CRC");
				putc_line('C');
				break;
			case NONE:
				waddstr(win, "NONE");
				putc_line('G');
				break;
		}
		/*
		 * We've cut the delay time in half, so we double
		 * the allowable errors
		 */
		if ((i = getc_line(5)) == -1) {
			err_count++;
			clear_line(win, 12, 24, TRUE);
			waddstr(win, "NO RESPONSE");
			wrefresh(win);
			continue;
		}
		buf[0] = i;
#ifdef DEBUG
		fprintf(stderr, "send_first: got header %02x, %03o, %d\n", buf[0], buf[0], buf[0]);
#endif /* DEBUG */

		switch (buf[0]) {
			case SOH:	/* small block follows */
				block_size = 128;
				return(0);
			case STX:	/* large block follows */
				if (max_block == 1024) {
					block_size = 1024;
					return(0);
				}
				/* fall thru */
			default:
				err_count++;
				clear_line(win, 12, 24, TRUE);
				waddstr(win, "BAD HEADER");
				wrefresh(win);
					/* read some garbage... */
				while(fread_line(buf, 1028, 1) != -1)
					;
				putc_line(NAK);
				break;
		}
	}
	beep();
	clear_line(win, 12, 24, TRUE);
	wattrstr(win, A_BOLD, "TIMED OUT");
	wrefresh(win);
	return(ERROR);
}

/*
 * Receive a block of info from the host.  Returns a 0 on success, a 1 on
 * a duplicate block or the standard error codes.  The variables
 * err_method and block_size are global.
 */

int
rcv_block(win, got_hdr, max_block, blk)
WINDOW *win;
int got_hdr, max_block;
unsigned char blk;
{
	int i, err_count, bad_block, out_of_sync;
	unsigned short crc, calc_crc();
	unsigned int packet, sleep();
	unsigned char blk_compliment, calc_sum(), crc_1, crc_2;
	void cancel_xfer();

	err_count = 0;
	while (err_count < MAX_ERRORS) {
		mvwprintw(win, 10, 24, "%-2d", err_count);
		mvwprintw(win, 11, 24, "%-3d", tot_err);

					/* scan the keyboard for abort */
		if (wgetch(win) == ESC) {
			beep();
			clear_line(win, 12, 24, TRUE);
			waddstr(win, "ABORTED");
			wrefresh(win);
			cancel_xfer(DOWN_LOAD);
			sleep(3);
			return(ABORT);
		}
					/* have we already got a hdr? */
		if (!got_hdr) {
			if ((i = getc_line(10)) == -1) {
				err_count++;
				tot_err++;
				clear_line(win, 12, 24, TRUE);
				waddstr(win, "NO RESPONSE");
				wrefresh(win);
				continue;
			}
			buf[0] = i;
#ifdef DEBUG
			fprintf(stderr, "rcv_block: got header %02x, %03o, %d\n", buf[0], buf[0], buf[0]);
#endif /* DEBUG */
					/* what'd we get? */
			switch (buf[0]) {
				case EOT:	/* we're done! */
					clear_line(win, 12, 24, TRUE);
					waddstr(win, "TRANSFER COMPLETE");
					wrefresh(win);
					sleep(1);
					return(0);
				case SOH:	/* small block follows */
					block_size = 128;
					break;
				case STX:	/* large block follows */
					if (max_block == 1024) {
						block_size = 1024;
						break;
					}
					/* fall thru... */
				default:
					err_count++;
					tot_err++;
					clear_line(win, 12, 24, TRUE);
					waddstr(win, "BAD HEADER");
					wrefresh(win);

					/* read some garbage... */
					while(fread_line(buf, 1028, 1) != -1)
						;
					putc_line(NAK);
					continue;
			}
		}
					/* read the rest of the packet */
		packet = block_size + 2 + (err_method == CHECKSUM ? 1 : 2);
		if (fread_line(&buf[1], packet, 10) == -1) {
			clear_line(win, 12, 24, TRUE);
			waddstr(win, "TIMED OUT");
			wrefresh(win);
			putc_line(NAK);
			err_count++;
			tot_err++;
			continue;
		}

		/*
		 * Validation of the packet includes checking the
		 * block number, its complement, and the crc/checksum.
		 */
		out_of_sync = 0;
		blk_compliment = ~blk;
		if (buf[1] != blk || buf[2] != blk_compliment)
			out_of_sync++;

		bad_block = 0;
		switch (err_method) {
			case CHECKSUM:
#ifdef DEBUG
				fprintf(stderr, "blk=%d, checksum=%d\n", blk, calc_sum(&buf[3], block_size));
#endif /* DEBUG */
				if (buf[block_size +3] != calc_sum(&buf[3], block_size))
					bad_block++;
				break;
			case CRC:
				crc = calc_crc(&buf[3], block_size);
				crc_1 = crc >> 8;
				crc_2 = crc;
#ifdef DEBUG
				fprintf(stderr, "blk=%d, crc1=%d, crc2=%d\n", blk, crc_1, crc_2);
#endif /* DEBUG */
				if (buf[block_size +3] != crc_1 || buf[block_size +4] != crc_2)
					bad_block++;
				break;
			case NONE:
				return(0);
		}
					/* handle errors */
		if (bad_block) {
			clear_line(win, 12, 24, TRUE);
			if (err_method == CRC)
				waddstr(win, "CRC FAILED");
			else
				waddstr(win, "CHECKSUM FAILED");
			wrefresh(win);
			putc_line(NAK);
			err_count++;
			tot_err++;
			continue;
		}
					/* not really an error */
		if (out_of_sync) {
			/*
			 * If a perfect packet is off by 1 block number,
			 * (a lost ACK could cause this) then treat it as
			 * a good block but don't write it to disk.
			 */
			if (buf[1] == (unsigned char) blk-1)
				return(1);

			clear_line(win, 12, 24, TRUE);
			waddstr(win, "OUT OF SYNC");
			wrefresh(win);
			putc_line(NAK);
			err_count++;
			tot_err++;
			continue;
		}
		return(0);
	}
	beep();
	clear_line(win, 12, 24, TRUE);
	waddstr(win, "TOO MANY ERRORS");
	wrefresh(win);
	cancel_xfer(DOWN_LOAD);
	return(ERROR);
}
