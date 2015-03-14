/*
 * xdm - display manager daemon
 *
 * $XConsortium: xdmauthgen.c,v 1.5 89/08/31 11:35:59 keith Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

/*
 * xdmauthgen
 *
 * generate cryptographically secure authorization keys
 * for MIT-MAGIC-COOKIE-1 type authentication
 */

# include   <X11/Xauth.h>
# include   <X11/Xos.h>

# define FILE_LIMIT	1024	/* no more than this many buffers */
# define AUTH_DATA_LEN	16	/* bytes of authorization data */
# define AUTH_NAME	"MIT-MAGIC-COOKIE-1"
# define AUTH_NAME_LEN	18

sumFile (name, sum)
char	*name;
long	sum[2];
{
    long    data[2];
    long    buf[1024*2];
    int	    cnt;
    int	    fd;
    int	    loops;
    int	    reads;
    int	    i;

    fd = open (name, 0);
    if (!fd)
	return 0;
    reads = FILE_LIMIT;
    while ((cnt = read (fd, buf, sizeof (buf))) > 0 && --reads > 0) {
	loops = cnt / sizeof (data);
	for (i = 0; i < loops; i+= 2) {
	    sum[0] += buf[i];
	    sum[1] += buf[i+1];
	}
    }
    return 1;
}

#ifndef DONT_USE_DES
# ifndef USE_CRYPT
#  ifdef ultrix
#   define USE_CRYPT
#  endif
#  ifdef hpux
#   define USE_CRYPT
#  endif
#  ifdef macII
#   define USE_CRYPT
#  endif
# endif
#endif

#if !defined (DONT_USE_DES) && !defined (USE_CRYPT)
# define USE_ENCRYPT
#endif

generateAuth (data, key, auth)
char	*data;
char	*key;
char	*auth;
{
#ifdef DONT_USE_DES
    int	    seed;
    int	    value;
    int	    i;

    seed = (data[0] + key[0]) +
	  ((data[1] + key[1]) << 8) +
 	  ((data[2] + key[2]) << 16) +
 	  ((data[3] + key[3]) << 24);
    srand (seed);
    for (i = 0; i < 16; i += 2)
    {
	value = rand ();
	auth[i] = value & 0xff;
	auth[i+1] = (value >> 8) & 0xff;
    }
    bcopy (auth, key, 8);
#else
#ifndef USE_ENCRYPT
    int	    i, j, k;
    char    *result, *crypt ();

    for (i = 0, j=0; i < 8; i+= 2, j+=4) {
	for (k = 0; k < 8; k++)
	    if (data[k] == 0)
		data[k] = 1;
	data[k] = 0;
	result = crypt (data, key+i);
	auth[j] = result[2];
	auth[j+1] = result[3];
	auth[j+2] = result[4];
	auth[j+3] = result[5];
	bcopy (result+2, data, 8);
    }
#else
    int	    bit;
    int	    i;

    setkey (key);
    for (i = 0; i < AUTH_DATA_LEN; i++) {
	auth[i] = 0;
	for (bit = 1; bit < 256; bit <<= 1) {
	    encrypt (data, 0);
	    if (data[0])
		auth[i] |= bit;
	}
    }
#endif
#endif
}

#ifdef USE_ENCRYPT
bitsToBytes (bits, bytes)
unsigned long	bits[2];
char	bytes[64];
{
    int	bit, byte;
    int	i;

    i = 0;
    for (byte = 0; byte < 2; byte++)
	for (bit = 0; bit < 32; bit++)
	    bytes[i++] = ((bits[byte] & (1 << bit)) != 0);
}
#endif

main (argc, argv)
char	**argv;
{
    char    *key_file = "/dev/mem";
    long    data[2];
    long    key[2];
    char    c;
    Xauth   local;
    char    key_bits[64];
    char    data_bits[64];
    char    auth_data[AUTH_DATA_LEN];
    
    if (argc > 1)
	key_file = argv[1];
    if (!sumFile (key_file, key)) {
	key[0] = time ((long *) 0);
	key[1] = time ((long *) 0);
    }
#ifdef USE_ENCRYPT
    bitsToBytes (key, key_bits);
#endif
    local.family = FamilyWild;
    local.address_length = 0;
    local.address = "";
    local.number_length = 0;
    local.number = "";
    local.name_length = AUTH_NAME_LEN;
    local.name = AUTH_NAME;
    local.data_length = AUTH_DATA_LEN;
    local.data = auth_data;
    for (;;) {
#ifdef ITIMER_REAL
	{
	    struct timeval  now;
	    struct timezone zone;
	    gettimeofday (&now, &zone);
	    data[0] = now.tv_sec;
	    data[1] = now.tv_usec;
	}
#else
	{
	    long    time ();

	    data[0] = time ((long *) 0);
	    data[1] = getpid ();
	}
#endif
#ifdef USE_ENCRYPT
	bitsToBytes (data, data_bits);
	generateAuth (data_bits, key_bits, auth_data);
#else
	generateAuth (data, key, auth_data);
#endif
	/*
	 * wait for a request before sending the packet
	 */
	if (read (0, &c, 1) != 1)
	    break;
	XauWriteAuth (stdout, &local);
	fflush (stdout);
    }
    exit (0);
}
