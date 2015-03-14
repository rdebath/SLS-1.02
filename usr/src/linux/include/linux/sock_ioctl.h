#ifndef _LINUX_SOCK_IOCTL_H
#define _LINUX_SOCK_IOCTL_H

#define MAX_IP_NAME 20
/* some ioctl.  Their values are not special. */
#define IP_SET_DEV 0x2401

struct ip_config
{
   char name[MAX_IP_NAME];
   unsigned long paddr;
   unsigned long router;
   unsigned long net;
   unsigned long up:1,destroy:1;
};

#define SIOCSARP	0x2501
#define SIOCGARP	0x2502
#define SIOCDARP	0x2503

/*
 * ARP ioctl request
 */
struct arpreq {
	struct	sockaddr arp_pa;		/* protocol address */
	struct	sockaddr arp_ha;		/* hardware address */
	int	arp_flags;			/* flags */
};

#define ATF_COM		0x02
#define ATF_PERM	0x04
#define ATF_PUBL	0x08
#define ATF_USETRAILERS	0x10

#endif
