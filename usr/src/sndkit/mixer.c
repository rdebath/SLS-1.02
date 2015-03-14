/*
 *	This is an example of a mixer program for Linux
 *
 *	updated 1/1/93 to add stereo, level query, broken
 *      	devmask kludge - cmetz@thor.tjhsst.edu 
 */

#include <stdio.h>
#include <fcntl.h>
#include <linux/soundcard.h>

char *names[SOUND_MIXER_NRDEVICES] = SOUND_DEVICE_NAMES;

int devmask = 0, recmask = 0, recsrc = 0;

void usage(void)
{
	int i, n = 0;
	printf("Usage: mixer { ");

	for (i = 0; i < SOUND_MIXER_NRDEVICES; i++) 
		if ((1 << i) & devmask)  {
			if (n)
				putchar('|');
			printf(names[i]);
			n = 1;
		} 
	printf(" } <value>\n  or   mixer { +rec|-rec } <devicename>\n");
	exit(1);
}

void print_recsrc(void)
{
	int i, n = 0;
	fprintf(stderr, "Recording source: ");

	for (i = 0; i < SOUND_MIXER_NRDEVICES; i++)
		if ((1 << i) & recsrc) {
			if (n)
				fprintf(stderr, ", ");
			fprintf(stderr, names[i]);
			n = 1;
		}
	fprintf(stderr, "\n");
}

void main(int argc, char *argv[])
{
	int foo, bar, baz, dev;

	if ((baz = open("/dev/mixer", O_RDWR)) < 0) {
		puts("Error opening /dev/mixer.");
		exit(1);
	}
	if ((devmask = ioctl(baz, SOUND_MIXER_READ_DEVMASK, 0)) == -1) {
		perror("SOUND_MIXER_READ_DEVMASK");
		exit(-1);
	}
	if ((recmask = ioctl(baz, SOUND_MIXER_READ_RECMASK, 0)) == -1) {
		perror("SOUND_MIXER_READ_RECMASK");
		exit(-1);
	}
	if ((recsrc = ioctl(baz, SOUND_MIXER_READ_RECSRC, 0)) == -1) {
		perror("SOUND_MIXER_READ_RECSRC");
		exit(-1);
	}

	switch (argc) {
		case 3:
			bar = 1;
			break;
		case 2:
			bar = 0;
			break;
		default:
			usage();
	}

	for (foo = 0; foo < SOUND_MIXER_NRDEVICES && strcmp(names[foo], argv[1]); foo++);

	if (foo >= SOUND_MIXER_NRDEVICES) {

		if (!strcmp("+rec", argv[1]) || !strcmp("-rec", argv[1])) {
			for (dev = 0; dev < SOUND_MIXER_NRDEVICES && strcmp(names[dev], argv[2]); dev++);
			if (dev >= SOUND_MIXER_NRDEVICES)
				usage();

			if (!((1 << dev) & recmask)) {
				fprintf(stderr, "Invalid recording source %s\n", argv[2]);
				exit(-1);
			}
			if (argv[1][0] == '+')
				recsrc |= (1 << dev);
			else
				recsrc &= ~(1 << dev);

			if ((recsrc = ioctl(baz, SOUND_MIXER_WRITE_RECSRC, recsrc)) == -1) {
				perror("SOUND_MIXER_WRITE_RECSRC");
				exit(-1);
			}
			print_recsrc();

		} else
			usage();
	} else {
		if (bar) {
			if (strchr(argv[2], ':') == NULL) {
				sscanf(argv[2], "%d", &bar);
				dev = bar;
			} else 
				sscanf(argv[2], "%d:%d", &bar, &dev);
	
			if (bar < 0)
				bar = 0;
			if (dev < 0)
				dev = 0;
			if (bar > 100)
				bar = 100;
			if (dev > 100)
				dev = 100;

			printf("Setting the mixer %s to %d:%d.\n", names[foo], bar, dev);

			if ((bar = ioctl(baz, SOUND_BASE | SOUND_MIXER | SOUND_WRITE | foo, bar | (dev << 8))) < 0)
				puts("Error setting mixer level.");
		} else {
			bar = ioctl(baz, SOUND_BASE | SOUND_MIXER | SOUND_READ | foo);
			printf("The mixer %s is currently set to %d:%d.\n", names[foo], bar & 0x7f, (bar >> 8) & 0x7f);
		}
	}

	close(baz);
}
