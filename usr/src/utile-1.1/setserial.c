/* setserial.c - get/set Linux serial port info - rick sladkey */
/* modified to do work again and added setting fast serial speeds,
   Michael K. Johnson, johnsonm@stolaf.edu */
/*
 * Very heavily modified --- almost rewritten from scratch --- to have
 * a more flexible command structure.  Now able to set any of the
 * serial-specific options using the TIOCSSERIAL ioctl().
 * 			Theodore Ts'o, tytso@mit.edu, 1/1/93
 */

#include <stdio.h>
#include <fcntl.h>
#include <termios.h>
#include <string.h>

#include <linux/fs.h>
#include <linux/tty.h>

char *progname;

struct serial_type_struct {
	int id;
	char *name;
} serial_type_tbl[] = {
	PORT_UNKNOWN,	"unknown",
	PORT_8250,	"8250",
	PORT_16450,	"16450",
	PORT_16550,	"16550",
	PORT_16550A,	"16550A",
	PORT_UNKNOWN,	"none",
	-1,		NULL
};

#define CMD_FLAG	1
#define CMD_PORT	2
#define CMD_IRQ		3
#define CMD_DIVISOR	4
#define CMD_TYPE	5
#define CMD_BASE	6

#define FLAG_CAN_INVERT	0x0001
#define FLAG_NEED_ARG	0x0002

struct flag_type_table {
	int	cmd;
	char	*name;
	int	bits;
	int	mask;
	int	flags;
} flag_type_tbl[] = {
	CMD_FLAG, 	"SAK", 		ASYNC_SAK, 	ASYNC_SAK, 	FLAG_CAN_INVERT,
	CMD_FLAG,	"FOURPORT",	ASYNC_FOURPORT, ASYNC_FOURPORT,	FLAG_CAN_INVERT,
	CMD_FLAG,	"SPD_NORMAL",	0,		ASYNC_SPD_MASK,		0,
	CMD_FLAG,	"SPD_HI",	ASYNC_SPD_HI, 	ASYNC_SPD_MASK, 0,
	CMD_FLAG,	"SPD_VHI",	ASYNC_SPD_VHI,	ASYNC_SPD_MASK,	0,
	CMD_FLAG,	"SPD_CUST",	ASYNC_SPD_CUST,	ASYNC_SPD_MASK,	0,
	CMD_PORT,	"port",		0,		0,		FLAG_NEED_ARG,
	CMD_IRQ,	"irq",		0,		0,		FLAG_NEED_ARG,
	CMD_DIVISOR,	"divisor",	0,		0,		FLAG_NEED_ARG,
	CMD_TYPE,	"uart",		0,		0,		FLAG_NEED_ARG,
	CMD_BASE,	"base",		0,		0,		FLAG_NEED_ARG,
	CMD_BASE,	"baud_base",	0,		0,		FLAG_NEED_ARG,
	0,		0,		0,		0,		0,
};
	
char *serial_type(int id)
{
	int i;

	for (i = 0; serial_type_tbl[i].id != -1; i++)
		if (id == serial_type_tbl[i].id)
			return serial_type_tbl[i].name;
	return "undefined";
}

int uart_type(char *name)
{
	int i;

	for (i = 0; serial_type_tbl[i].id != -1; i++)
		if (!strcasecmp(name, serial_type_tbl[i].name))
			return serial_type_tbl[i].id;
	return -1;
}


int atonum(char *s)
{
	int n;

	while (*s == ' ')
		s++;
	if (strncmp(s, "0x", 2) == 0 || strncmp(s, "0X", 2) == 0)
		sscanf(s + 2, "%x", &n);
	else if (s[0] == '0' && s[1])
		sscanf(s + 1, "%o", &n);
	else
		sscanf(s, "%d", &n);
	return n;
}

void print_flags(struct serial_struct *serinfo)
{
	struct	flag_type_table	*p;
	int	flags;

	flags = serinfo->flags;
	
	for (p = flag_type_tbl; p->name; p++) {
		if (p->cmd != CMD_FLAG)
			continue;
		if ((flags & p->mask) == p->bits)
			printf("%s ", p->name);
	}
	printf("\n");
}

void get_serial(char *device)
{
	struct serial_struct serinfo;
	int	fd;

	if ((fd = open(device, O_RDWR)) < 0) {
		perror(device);
		return;
	}
	if (ioctl(fd, TIOCGSERIAL, &serinfo) < 0) {
		perror("Cannot get serial info");
		close(fd);
		return;
	}
	if (serinfo.irq == 9)
		serinfo.irq = 2;	/* People understand 2 better than 9 */
	printf("%s, Line %d, UART: %s, Port: 0x%.4x, IRQ: %d, Baud_base: %d, Flags: ",
	       device, serinfo.line, serial_type(serinfo.type),
	       serinfo.port, serinfo.irq, serinfo.baud_base);
	print_flags(&serinfo);
	close(fd);
}

void set_serial(char *device, char ** arg)
{
	struct serial_struct old_serinfo, new_serinfo;
	struct	flag_type_table	*p;
	int	fd;
	int	do_invert = 0;
	char	*word;
	

	if ((fd = open(device, O_RDWR)) < 0) {
		perror(device);
		exit(1);
	}
	if (ioctl(fd, TIOCGSERIAL, &old_serinfo) < 0) {
		perror("Cannot get serial info");
		exit(1);
	}
	new_serinfo = old_serinfo;
	while (*arg) {
		do_invert = 0;
		word = *arg++;
		if (*word == '-') {
			do_invert++;
			word++;
		}
		for (p = flag_type_tbl; p->name; p++) {
			if (!strcasecmp(p->name, word))
				break;
		}
		if (!p->name) {
			fprintf(stderr, "Invalid flag: %s\n", word);
			exit(1);
		}
		if (do_invert && !(p->flags & FLAG_CAN_INVERT)) {
			fprintf(stderr, "This flag can not be inverted: %s\n", word);
			exit(1);
		}
		if ((p->flags & FLAG_NEED_ARG) && !*arg) {
			fprintf(stderr, "Missing argument for %s\n", word);
			exit(1);
		}
		switch (p->cmd) {
		case CMD_FLAG:
			new_serinfo.flags &= ~p->mask;
			if (!do_invert)
				new_serinfo.flags |= p->bits;
			break;
		case CMD_PORT:
			new_serinfo.port = atonum(*arg++);
			break;
		case CMD_IRQ:
			new_serinfo.irq = atonum(*arg++);
			break;
		case CMD_DIVISOR:
			new_serinfo.custom_divisor = atonum(*arg++);
			break;
		case CMD_TYPE:
			new_serinfo.type = uart_type(*arg++);
			if (new_serinfo.type < 0) {
				fprintf(stderr, "Illegal UART type: %s", *--arg);
				exit(1);
			}
			break;
		case CMD_BASE:
			new_serinfo.baud_base = atonum(*arg++);
			break;	
		default:
			fprintf(stderr, "Internal error: unhandled cmd #%d\n", p->cmd);
			exit(1);
		}
			
	}
	if (ioctl(fd, TIOCSSERIAL, &new_serinfo) < 0) {
		perror("Cannot set serial info");
		exit(1);
	}
}


void usage()
{
	fprintf(stderr,
		"usage: %s serial-device [opt1 [arg]] [opt2] ... \n\n", progname);
	fprintf(stderr, "Available options: (* = Takes an argument)\n");
	fprintf(stderr, "\t* port\t\tset the I/O port\n");
	fprintf(stderr, "\t* irq\t\tset the interrupt\n");	
	fprintf(stderr, "\t* uart\t\tset UART type (none, 8250, 16450, 16550, 16550A\n");
	fprintf(stderr, "\t* baud_base\tset base baud rate (CLOCK_FREQ / 16)\n");
	fprintf(stderr, "\t* divisor\tset the custom divisor (see below)\n");
	fprintf(stderr, "\t  sak\t\tset the break key as the Secure Attention Key\n");
	fprintf(stderr, "\t  -sak\t\tdisable the Secure Attention Key\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "\t  spd_hi\tuse 56kb instead of 38.4kb\n");
	fprintf(stderr, "\t  spd_vhi\tuse 115kb instead of 38.4kb\n");
	fprintf(stderr, "\t  spd_cust\tuse the custom divisor to set the speed at 38.4kb\n");
	fprintf(stderr, "\t\t\t\t(baud rate = baud_base / custom_divisor)\n");
	fprintf(stderr, "\t  spd_normal\tuse 38.4kb when a buad rate of 38.4kb is selected\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "Use a leading '0x' for hex numbers.\n");
	fprintf(stderr, "CAUTION: Using an invalid port can lock up your machine!\n");
	exit(1);
}

main(int argc, char **argv)
{
	progname = argv[0];
	if (argc == 1)
		usage();
	if (!strcmp(argv[1], "-g")) {
		argv += 2;
		while (*argv)
			get_serial(*argv++);
	} else if (argc == 2)
		get_serial(argv[1]);
	else
		set_serial(argv[1], argv+2);
	exit(0);
}

