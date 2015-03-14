/*
 * Send a list of files using a version of Ward Christensen's file
 * transfer protocol.  A non-zero return code means an error must be
 * acknowledged by the user (a user generated abort returns a 0).
 */

#include <stdio.h>
#include <curses.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "xmodem.h"

static int tot_err, err_method;

int
send_xmodem(win, list, type, fast)
WINDOW *win;
char *list;
int type, fast;
{
	extern char *protocol[];
	FILE *fp, *my_fopen();
	int i, block_size, file_count, secs, mins, hours, big_blocks;
	int small_blocks, err_count, got_it, num, is_batch, code;
	int max_block, default_err;
	long size, block, sent, xmit_size;
	char *file, *strtok(), *name, *strrchr();
	unsigned short crc, calc_crc();
	unsigned char buf[1029], blk, calc_sum();
	unsigned int packet, sleep();
	float performance, percent;
	struct stat stbuf;
					/* which protocol? */
	switch (type) {
		case XMODEM:
			is_batch = 0;
			default_err = CRC_CHECKSUM;
			max_block = 128;
			performance = 1.36;
			break;
		case XMODEM_1k:
			is_batch = 0;
			default_err = CRC_CHECKSUM;
			max_block = 1024;
			performance = 1.09;
			break;
		case MODEM7:
			is_batch = 1;
			default_err = CHECKSUM;
			max_block = 128;
			performance = 1.36;
			break;
		case YMODEM:
			is_batch = 1;
			default_err = CRC;
			max_block = 1024;
			performance = 1.09;
			break;
		case YMODEM_G:
			is_batch = 1;
			default_err = NONE;
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

					/* each one in the list */
	file = strtok(list, " \t");
	do {
					/* is it a batch type? */
		file_count++;
		if (file_count > 1 && !is_batch)
			break;
					/* display the name */
		clear_line(win, 3, 24, TRUE);
		if ((name = strrchr(file, '/')))
			name++;
		else
			name = file;
		waddstr(win, name);
		wrefresh(win);
					/* get the file size */
		if (stat(file, &stbuf) < 0) {
			beep();
			clear_line(win, 12, 24, TRUE);
			wattrstr(win, A_BOLD, "CAN'T FIND FILE");
			wrefresh(win);
			sleep(3);
			continue;
		}
					/* sanity checking */
		if ((stbuf.st_mode & S_IFREG) != S_IFREG) {
			beep();
			clear_line(win, 12, 24, TRUE);
			wattrstr(win, A_BOLD, "NOT REGULAR FILE");
			wrefresh(win);
			sleep(3);
			continue;
		}

		size = stbuf.st_size;
		mvwprintw(win, 4, 24, "%-10ld", size);
		clear_line(win, 5, 24, TRUE);

		if (!(fp = my_fopen(file, "r"))) {
			beep();
			clear_line(win, 12, 24, TRUE);
			wattrstr(win, A_BOLD, "PERMISSION DENIED");
			wrefresh(win);
			sleep(3);
			continue;
		}
					/* get the xmit size */
		block_size = max_block;
		big_blocks = 0;
		small_blocks = 0;
		if (block_size == 128) {
			small_blocks = size / 128;
			if (size % 128)
				small_blocks++;
		}
		else {
			big_blocks = size / 1024;
			small_blocks = (size % 1024) / 128;
			if (size % 128)
				small_blocks++;

			if (small_blocks == 8 && !big_blocks) {
				big_blocks++;
				small_blocks = 0;
			}
					/* if tiny file */
			if (big_blocks == 0)
				block_size = 128;
		}

		xmit_size = ((unsigned int) big_blocks * 1024L) + ((unsigned int) small_blocks * 128L);
					/* add block 0 to the size */
		if (type == YMODEM || type == YMODEM_G)
			xmit_size += 128L;

		secs = (xmit_size * 10.0 / dir->baud[dir->d_cur]) * performance;
		hours = secs / 3600;
		mins = (secs % 3600) / 60;
		secs = (secs % 3600) % 60;

		mvwprintw(win, 6, 24, "%d:%02d:%02d", hours, mins, secs);

					/* some starting numbers */
		mvwaddstr(win, 7, 24, "     ");
		mvwaddstr(win, 8, 24, "0%  ");
		mvwaddstr(win, 9, 24, "0          ");
		mvwaddstr(win, 10, 24, "0 ");
		clear_line(win, 12, 24, TRUE);
		waddstr(win, "NONE");
		wrefresh(win);
					/* send the batch stuff */
		switch (type) {
			case MODEM7:
				if (code = rcv_first(win, default_err)) {
					fclose(fp);
					return(code +1);
				}

				if (send_modem7(win, name)) {
					fclose(fp);
					return(1);
				}
				break;
			case YMODEM:
			case YMODEM_G:
				if (code = rcv_first(win, default_err)) {
					fclose(fp);
					return(code +1);
				}

				if (code = send_ymodem(win, name, size)) {
					fclose(fp);
					/*
					 * CANCEL now means that the other
					 * end can't open that file.
					 */
					if (code == CANCEL)
						break;
					return(code +1);
				}
				xmit_size -= 128L;
				break;
			default:
				code = 0;
				break;
		}
					/* remote can't receive that file? */
		if (code == CANCEL)
			break;
					/* wait for first character */
		if (code = rcv_first(win, default_err)) {
			fclose(fp);
			return(code +1);
		}
					/* here we go... */
		clear_line(win, 12, 24, TRUE);
		waddstr(win, "NONE");
		wrefresh(win);
		sent = 0L;
		block = 1L;
		blk = 1;
		while (num = fread((char *) &buf[3], sizeof(char), block_size, fp)) {

					/* fill short block */
			if (num < block_size) {
				for (i=num; i<block_size; i++)
					buf[i+3] = CTRLZ;
			}

					/* show current stats */
			mvwprintw(win, 7, 24, "%-5ld", block);
			if (fast) {
				percent = sent * 100.0 / xmit_size;
				mvwprintw(win, 8, 24, "%0.1f%%", percent);
				mvwprintw(win, 9, 24, "%-10ld", sent);
			}
			wrefresh(win);

					/* build the header */
			if (block_size == 128)
				buf[0] = SOH;
			else
				buf[0] = STX;

			buf[1] = blk;
			buf[2] = ~blk;

					/* build the error detection stuff */
			switch (err_method) {
				case CHECKSUM:
					buf[block_size+3] = calc_sum(&buf[3], block_size);
#ifdef DEBUG
					fprintf(stderr, "blk=%d, checksum=%d\n", blk, buf[block_size+3]);
#endif /* DEBUG */
					packet = block_size +4;
					break;
				case CRC:
					crc = calc_crc(&buf[3], block_size);
					buf[block_size+3] = crc >> 8;
					buf[block_size+4] = crc;
#ifdef DEBUG
					fprintf(stderr, "blk=%d, crc1=%d, crc2=%d\n", blk, buf[block_size+3], buf[block_size+4]);
#endif /* DEBUG */
					packet = block_size +5;
					break;
				case NONE:
					buf[block_size+3] = 0;
					buf[block_size+4] = 0;
					packet = block_size +5;
					break;
			}

					/* send the block */
			if (code = send_block(win, buf, packet)) {
				fclose(fp);
				return(code +1);
			}
			block++;
			blk++;
			sent = sent + (unsigned int) block_size;

					/* change block size? */
			if (xmit_size - sent < 1024)
				block_size = 128;
		}
		mvwaddstr(win, 8, 24, "100%  ");
		mvwprintw(win, 9, 24, "%-10ld", sent);
					/* at the end of the file */
		err_count = 0;
		got_it = 0;
		while (err_count < MAX_ERRORS) {
			putc_line(EOT);
			if (getc_line(10) == ACK) {
				got_it++;
				break;
			}
			err_count++;
		}
		clear_line(win, 12, 24, TRUE);
		if (!got_it) {
			/*
			 * So what???  We don't do anything if there is
			 * no acknowledge from the host!!
			 */
			waddstr(win, "NO ACKNOWLEDGE");
		}
		else
			waddstr(win, "TRANSFER COMPLETE");
		if (!is_batch)
			beep();
		wrefresh(win);
		sleep(2);
					/* prepare to start again */
		fclose(fp);
	} while (file = strtok((char *) NULL, " \t"));

	/*
	 * The end of batch markers... For modem7 it's an ACK and EOT, for
	 * ymodem, it's an empty block 0.
	 */
	switch (type) {
		case MODEM7:
			if (code = rcv_first(win, default_err))
				return(code +1);
			putc_line(ACK);
			putc_line(EOT);
			beep();
			wrefresh(win);
			break;
		case YMODEM:
		case YMODEM_G:
			if (code = rcv_first(win, default_err))
				return(code +1);

			if (code = send_ymodem(win, "", 0L))
				return(code +1);
			beep();
			wrefresh(win);
			break;
		default:
			break;
	}
	return(0);
}

/*
 * Wait for the first character to start the transmission.  This first
 * character also sets the crc/checksum method.  Returns the standard
 * error codes, or 0 on success.  The variable err_method is global.
 */

static int
rcv_first(win, default_err)
WINDOW *win;
int default_err;
{
	int i, err_count;
	unsigned int sleep();
	void cancel_xfer();

	err_count = 0;
	while (err_count < MAX_ERRORS) {

					/* scan the keyboard for abort */
		if (wgetch(win) == ESC) {
			beep();
			clear_line(win, 12, 24, TRUE);
			waddstr(win, "ABORTED");
			wrefresh(win);
			cancel_xfer(UP_LOAD);
			sleep(3);
			return(ABORT);
		}
					/* scan the TTY line */
		i = getc_line(10);
#ifdef DEBUG
		fprintf(stderr, "rcv_first: got \"%c\", %02x, %03o, %d\n", i, i, i, i);
#endif /* DEBUG */
		switch (i) {
			case -1:	/* timed out */
				clear_line(win, 12, 24, TRUE);
				wattrstr(win, A_BOLD, "NO RESPONSE");
				err_count++;
				break;
			case NAK:	/* checksum marker */
				if (default_err == CHECKSUM || default_err == CRC_CHECKSUM) {
					mvwaddstr(win, 5, 24, "CHECKSUM");
					err_method = CHECKSUM;
					return(0);
				}
				err_count++;
				break;
			case 'C':	/* CRC marker */
				if (default_err == CRC_CHECKSUM || default_err == CRC) {
					mvwaddstr(win, 5, 24, "CRC");
					err_method = CRC;
					return(0);
				}
				err_count++;
				break;
			case 'G':	/* ymodem-g marker */
				if (default_err == NONE) {
					mvwaddstr(win, 5, 24, "NONE");
					err_method = NONE;
					return(0);
				}
				err_count++;
				break;
			case CAN:	/* two CAN's and you're out! */
				if (getc_line(2) == CAN) {
					beep();
					clear_line(win, 12, 24, TRUE);
					wattrstr(win, A_BOLD, "REMOTE ABORTED");
					wrefresh(win);
					return(CANCEL);
				}
				err_count++;
				break;
			default:
				clear_line(win, 12, 24, TRUE);
				waddstr(win, "BAD HEADER");
				err_count++;
				break;
		}
		mvwprintw(win, 10, 24, "%-2d", err_count);
		wrefresh(win);
	}
					/* failed to get it right? */
	beep();
	clear_line(win, 12, 24, TRUE);
	wattrstr(win, A_BOLD, "TIMED OUT");
	wrefresh(win);
	return(ERROR);
}

/*
 * Send a block of data, scan the keyboard for a user abort, and check
 * the return codes from the host.  Returns standard error codes or 0
 * on success.
 */

int
send_block(win, blk, packet)
WINDOW *win;
unsigned char *blk;
unsigned int packet;
{
	extern int fd;
	int i, err_count;
	void cancel_xfer();

	err_count = 0;
	mvwaddstr(win, 10, 24, "0 ");

	while (err_count < MAX_ERRORS) {
					/* write the block */
		write(fd, (char *) blk, packet);
					/* scan the keyboard */
		if (wgetch(win) == ESC) {
			beep();
			clear_line(win, 12, 24, TRUE);
			waddstr(win, "ABORTED");
			wrefresh(win);
			cancel_xfer(UP_LOAD);
			sleep(3);
			return(ABORT);
		}
					/* ymodem-g doesn't need ACKs */
		if (err_method == NONE)
			return(0);
					/* wait for acknowledge */
		i = getc_line(10);
#ifdef DEBUG
		fprintf(stderr, "send_block: got \"%c\", %02x, %03o, %d\n", i, i, i, i);
#endif /* DEBUG */
		switch (i) {
			case -1:	/* timed out */
				clear_line(win, 12, 24, TRUE);
				waddstr(win, "NO RESPONSE");
				err_count++;
				tot_err++;
				break;
			case ACK:	/* Hooray!! we got it */
				return(0);
			case NAK:	/* show our disappointment... */
				clear_line(win, 12, 24, TRUE);
				if (err_method == CRC)
					waddstr(win, "CRC FAILED");
				else
					waddstr(win, "CHECKSUM FAILED");
				err_count++;
				tot_err++;
				break;
			case CAN:	/* two CAN's and you're out! */
				if (getc_line(2) == CAN) {
					beep();
					clear_line(win, 12, 24, TRUE);
					wattrstr(win, A_BOLD, "REMOTE ABORTED");
					wrefresh(win);
					return(CANCEL);
				}
				/* fall thru... */
			default:
				clear_line(win, 12, 24, TRUE);
				waddstr(win, "RESENDING");
				err_count++;
				tot_err++;
				break;
		}
					/* flush any pending garbage */
		tty_flush(fd, 0);

		mvwprintw(win, 10, 24, "%-2d", err_count);
		mvwprintw(win, 11, 24, "%-3d", tot_err);
		wrefresh(win);
	}
					/* failed to get it right */
	beep();
	clear_line(win, 12, 24, TRUE);
	wattrstr(win, A_BOLD, "TOO MANY ERRORS");
	wrefresh(win);
	cancel_xfer(UP_LOAD);
	return(ERROR);
}
