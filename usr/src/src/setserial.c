/* setserial.c - get/set Linux serial port info - rick sladkey */
/* modified to do work again and added setting fast serial speeds,
   Michael K. Johnson, johnsonm@stolaf.edu */

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
	-1,		NULL
};
	
char *serial_type(int id)
{
	int i;

	for (i = 0; serial_type_tbl[i].id != -1; i++)
		if (id == serial_type_tbl[i].id)
			return serial_type_tbl[i].name;
	return "undefined";
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

void getserial(char *device, int fd)
{
	struct serial_struct serinfo;

	if (ioctl(fd, TIOCGSERIAL, &serinfo) < 0) {
		perror("Cannot get serial info");
		exit(1);
	}
	printf("%s, Type: %s, Line: %d, Port: 0x%.4x, IRQ: %d\n",
		device, serial_type(serinfo.type),
		serinfo.line, serinfo.port, serinfo.irq);
}

void setserial(char *device, int fd, int port, int irq)
{
	struct serial_struct old_serinfo, new_serinfo;

	if (ioctl(fd, TIOCGSERIAL, &old_serinfo) < 0) {
		perror("Cannot get serial info");
		exit(1);
	}
	new_serinfo = old_serinfo;
	new_serinfo.port = port;
	new_serinfo.irq = irq;
	if (ioctl(fd, TIOCSSERIAL, &new_serinfo) < 0) {
		perror("Cannot set serial info");
		exit(1);
	}
	if (ioctl(fd, TIOCGSERIAL, &new_serinfo) < 0) {
		perror("Cannot get serial info");
		exit(1);
	}
	printf("%s, Type: %s, Line: %d, "
		"Port: 0x%.4x (was 0x%.4x), IRQ: %d (was %d)\n",
		device, serial_type(new_serinfo.type),
		new_serinfo.line, new_serinfo.port, old_serinfo.port,
		new_serinfo.irq, old_serinfo.irq);
}


void set_speed(char *device, int fd, char * arg)
{
	struct serial_struct old_serinfo, new_serinfo;

	if (ioctl(fd, TIOCGSERIAL, &old_serinfo) < 0) {
		perror("Cannot get serial info");
		exit(1);
	}
	new_serinfo = old_serinfo;
	new_serinfo.flags &= ~ASYNC_SPD_MASK;
	if (strcmp(arg, "SPD_HI") == 0)
		new_serinfo.flags |= ASYNC_SPD_HI; /* Use 56000 instead of 38400 */
	else if (strcmp(arg, "SPD_VHI") == 0)
		new_serinfo.flags |= ASYNC_SPD_VHI; /* Use 115200 */
	else {
		fprintf(stderr, "Invalid speed: use SPD_HI or SPD_VHI\n");
		exit(1);
	}

	if (ioctl(fd, TIOCSSERIAL, &new_serinfo) < 0) {
		perror("Cannot set serial info");
		exit(1);
	}
}




main(int argc, char **argv)
{
	char *device;
	int fd;

	progname = argv[0];
	if (argc == 1) {
		fprintf(stderr,
			"usage: %s serial-device [ port  irq ] [ speed ] \n",
			progname);
		fprintf(stderr, "for example: %s /dev/ttys3 0x03e8 5\n",
			progname);
		fprintf(stderr, "         or: %s /dev/ttys3 SPD_HI\n",
			progname);
		fprintf(stderr, "Use a leading '0x' for hex numbers.\n");
		fprintf(stderr, "CAUTION: "
			"Using an invalid port can lock up your machine!\n");
		fprintf(stderr,
			"Speed can be SPD_HI (56000) or SPD_VHI (115000)\n", 
			progname);
		exit(1);
	}
	device = argv[1];
	if ((fd = open(device, O_RDWR)) < 0) {
		perror("Cannot open serial device");
		exit(1);
	}
	if (argc == 2)
		getserial(device, fd);
	else if (argc == 3)
		set_speed(device, fd, argv[2]);
	else if (argc == 4)
		setserial(device, fd, atonum(argv[2]), atonum(argv[3]));
	else {
		fprintf(stderr, "%s: wrong number of arguments\n", progname);
		exit(1);
	}
	exit(0);
}

