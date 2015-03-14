/****************************************************************
Copyright 1990 by AT&T Bell Laboratories and Bellcore.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the names of AT&T Bell Laboratories or
Bellcore or any of their entities not be used in advertising or
publicity pertaining to distribution of the software without
specific, written prior permission.

AT&T and Bellcore disclaim all warranties with regard to this
software, including all implied warranties of merchantability
and fitness.  In no event shall AT&T or Bellcore be liable for
any special, indirect or consequential damages or any damages
whatsoever resulting from loss of use, data or profits, whether
in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of
this software.
****************************************************************/

#include "stdio.h"

 char *progname;

 void
usage(rc)
{
	fprintf(stderr, "usage: %s [file [file...]]\n", progname);
	exit(rc);
	}

main(argc, argv)
 char **argv;
{
	int x;
	char *s;
	static int rc;

	progname = *argv;
	s = *++argv;
	if (s && *s == '-') {
		switch(s[1]) {
			case '?':
				usage(0);
			case '-':
				break;
			default:
				fprintf(stderr, "invalid option %s\n", s);
				usage(1);
			}
		s = *++argv;
		}
	if (s) do {
		x = open(s,0);
		if (x < 0) {
			fprintf(stderr, "%s: can't open %s\n", progname, s);
			rc |= 1;
			}
		else
			process(s, x);
		}
		while(s = *++argv);
	else {
		process("/dev/stdin", fileno(stdin));
		}
	exit(rc);
	}

typedef unsigned char uchar;

 long
sum32(sum, x, n)
 register long sum;
 register uchar *x;
 int n;
{
	register uchar *xe;
	static long crc_table[256] = {
		0x0,		0x9073096,	0x120e612c,	0x1b0951ba,
		0xff6dc419,	0xf66af48f,	0xed63a535,	0xe46495a3,
		0xfedb8832,	0xf7dcb8a4,	0xecd5e91e,	0xe5d2d988,
		0x1b64c2b,	0x8b17cbd,	0x13b82d07,	0x1abf1d91,
		0xfdb71064,	0xf4b020f2,	0xefb97148,	0xe6be41de,
		0x2dad47d,	0xbdde4eb,	0x10d4b551,	0x19d385c7,
		0x36c9856,	0xa6ba8c0,	0x1162f97a,	0x1865c9ec,
		0xfc015c4f,	0xf5066cd9,	0xee0f3d63,	0xe7080df5,
		0xfb6e20c8,	0xf269105e,	0xe96041e4,	0xe0677172,
		0x403e4d1,	0xd04d447,	0x160d85fd,	0x1f0ab56b,
		0x5b5a8fa,	0xcb2986c,	0x17bbc9d6,	0x1ebcf940,
		0xfad86ce3,	0xf3df5c75,	0xe8d60dcf,	0xe1d13d59,
		0x6d930ac,	0xfde003a,	0x14d75180,	0x1dd06116,
		0xf9b4f4b5,	0xf0b3c423,	0xebba9599,	0xe2bda50f,
		0xf802b89e,	0xf1058808,	0xea0cd9b2,	0xe30be924,
		0x76f7c87,	0xe684c11,	0x15611dab,	0x1c662d3d,
		0xf6dc4190,	0xffdb7106,	0xe4d220bc,	0xedd5102a,
		0x9b18589,	0xb6b51f,	0x1bbfe4a5,	0x12b8d433,
		0x807c9a2,	0x100f934,	0x1a09a88e,	0x130e9818,
		0xf76a0dbb,	0xfe6d3d2d,	0xe5646c97,	0xec635c01,
		0xb6b51f4,	0x26c6162,	0x196530d8,	0x1062004e,
		0xf40695ed,	0xfd01a57b,	0xe608f4c1,	0xef0fc457,
		0xf5b0d9c6,	0xfcb7e950,	0xe7beb8ea,	0xeeb9887c,
		0xadd1ddf,	0x3da2d49,	0x18d37cf3,	0x11d44c65,
		0xdb26158,	0x4b551ce,	0x1fbc0074,	0x16bb30e2,
		0xf2dfa541,	0xfbd895d7,	0xe0d1c46d,	0xe9d6f4fb,
		0xf369e96a,	0xfa6ed9fc,	0xe1678846,	0xe860b8d0,
		0xc042d73,	0x5031de5,	0x1e0a4c5f,	0x170d7cc9,
		0xf005713c,	0xf90241aa,	0xe20b1010,	0xeb0c2086,
		0xf68b525,	0x66f85b3,	0x1d66d409,	0x1461e49f,
		0xedef90e,	0x7d9c998,	0x1cd09822,	0x15d7a8b4,
		0xf1b33d17,	0xf8b40d81,	0xe3bd5c3b,	0xeaba6cad,
		0xedb88320,	0xe4bfb3b6,	0xffb6e20c,	0xf6b1d29a,
		0x12d54739,	0x1bd277af,	0xdb2615,	0x9dc1683,
		0x13630b12,	0x1a643b84,	0x16d6a3e,	0x86a5aa8,
		0xec0ecf0b,	0xe509ff9d,	0xfe00ae27,	0xf7079eb1,
		0x100f9344,	0x1908a3d2,	0x201f268,	0xb06c2fe,
		0xef62575d,	0xe66567cb,	0xfd6c3671,	0xf46b06e7,
		0xeed41b76,	0xe7d32be0,	0xfcda7a5a,	0xf5dd4acc,
		0x11b9df6f,	0x18beeff9,	0x3b7be43,	0xab08ed5,
		0x16d6a3e8,	0x1fd1937e,	0x4d8c2c4,	0xddff252,
		0xe9bb67f1,	0xe0bc5767,	0xfbb506dd,	0xf2b2364b,
		0xe80d2bda,	0xe10a1b4c,	0xfa034af6,	0xf3047a60,
		0x1760efc3,	0x1e67df55,	0x56e8eef,	0xc69be79,
		0xeb61b38c,	0xe266831a,	0xf96fd2a0,	0xf068e236,
		0x140c7795,	0x1d0b4703,	0x60216b9,	0xf05262f,
		0x15ba3bbe,	0x1cbd0b28,	0x7b45a92,	0xeb36a04,
		0xead7ffa7,	0xe3d0cf31,	0xf8d99e8b,	0xf1deae1d,
		0x1b64c2b0,	0x1263f226,	0x96aa39c,	0x6d930a,
		0xe40906a9,	0xed0e363f,	0xf6076785,	0xff005713,
		0xe5bf4a82,	0xecb87a14,	0xf7b12bae,	0xfeb61b38,
		0x1ad28e9b,	0x13d5be0d,	0x8dcefb7,	0x1dbdf21,
		0xe6d3d2d4,	0xefd4e242,	0xf4ddb3f8,	0xfdda836e,
		0x19be16cd,	0x10b9265b,	0xbb077e1,	0x2b74777,
		0x18085ae6,	0x110f6a70,	0xa063bca,	0x3010b5c,
		0xe7659eff,	0xee62ae69,	0xf56bffd3,	0xfc6ccf45,
		0xe00ae278,	0xe90dd2ee,	0xf2048354,	0xfb03b3c2,
		0x1f672661,	0x166016f7,	0xd69474d,	0x46e77db,
		0x1ed16a4a,	0x17d65adc,	0xcdf0b66,	0x5d83bf0,
		0xe1bcae53,	0xe8bb9ec5,	0xf3b2cf7f,	0xfab5ffe9,
		0x1dbdf21c,	0x14bac28a,	0xfb39330,	0x6b4a3a6,
		0xe2d03605,	0xebd70693,	0xf0de5729,	0xf9d967bf,
		0xe3667a2e,	0xea614ab8,	0xf1681b02,	0xf86f2b94,
		0x1c0bbe37,	0x150c8ea1,	0xe05df1b,	0x702ef8d
		};

	xe = x + n;
	while(x < xe)
		sum = crc_table[(sum ^ *x++) & 0xff] ^ (sum >> 8 & 0xffffff);
	return sum;
	}

process(s, x)
 char *s;
 int x;
{
	register int n;
	uchar buf[64*1024];
	long fsize, sum;

	sum = 0;
	fsize = 0;
	while((n = read(x, (char *)buf, sizeof(buf))) > 0) {
		fsize += n;
		sum = sum32(sum, buf, n);
		}
	sum &= 0xffffffff;
        if (n==0)
		printf("%s\t%lx\t%ld\n", s, sum & 0xffffffff, fsize);
        else { perror(s); }
	close(x);
	return(0);
	}
