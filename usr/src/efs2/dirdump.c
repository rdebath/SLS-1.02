/*
 * dirdump.c - dumps an ext 2 fs directory
 *
 * Copyright (C) 1992, 1993  Remy Card (card@masi.ibp.fr)
 *
 */

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#include <linux/fs.h>
#include <linux/ext2_fs.h>

void main (int argc, char **argv)
{
	struct hdr
	{
		unsigned long inode;
		unsigned short rec_len;
		unsigned short name_len;
	} hdr;
	char name [256];
	int h;
	int offset = 0;
	int n;

	fprintf (stderr, "dirdump 0.2d, 93/03/30 for EXT2 FS %s\n",
		 EXT2FS_VERSION);
	if (argc != 2)
	{
		printf ("Usage: dirdump directory\n");
		exit (1);
	}
	if ((h = open (argv[1], O_RDONLY)) < 0)
	{
		perror ("open directory");
		exit (2);
	}
	while (1)
	{
		n = read (h, (char *) &hdr, sizeof (hdr));
		if (n == -1)
		{
			perror ("reading header");
			exit (3);
		}
		if (n == 0)
			exit (0);
		if (n < sizeof (hdr))
		{
			printf ("Error reading header, read = %d\n", n);
			exit (4);
		}
		printf ("%4d (%04x): inode = %4d, name_len = %4d, rec_len = %4d, name = ",
			offset, offset, hdr.inode, hdr.name_len, hdr.rec_len);
		n = read (h, name, hdr.name_len);
		if (n == -1)
		{
			perror ("reading name");
			exit (5);
		}
		if (n < hdr.name_len)
		{
			printf ("Error reading name, read = %d\n", n);
			exit (6);
		}
		if (hdr.inode)
		{
			name[hdr.name_len] = 0;
			printf ("%s\n", name);
		}
		else
			printf ("** Unused **\n");
		if (hdr.rec_len %4 != 0)
		{
			printf ("Record length %%4 != 0, skipping block\n");
			offset = ((offset / 1024) + 1) * 1024;
		}
		else if (hdr.rec_len < hdr.name_len + 8)
		{
			printf ("Record length < Name length + 8, skipping block\n");
			offset = ((offset / 1024) + 1) * 1024;
		}
		else if (hdr.rec_len == 0)
		{
			printf ("Record length == 0, skipping block\n");
			offset = ((offset / 1024) + 1) * 1024;
		}
		else if (((offset + hdr.rec_len - 1) / 1024) > (offset / 1024))
		{
			printf ("Directory entry across two blocks, skipping\n");
			offset = ((offset / 1024) + 1) * 1024;
		}
		else
			offset += hdr.rec_len;
		if (lseek (h, offset, SEEK_SET) < 0)
		{
			perror ("lseek");
			exit (7);
		}
	}
	exit (0);
}
