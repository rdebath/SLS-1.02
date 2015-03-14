#include <ansidecl.h>
#include <gnu-stabs.h>
#include <netinet/in.h>

#undef ntohl
#undef ntohs
#undef htonl
#undef htons

function_alias(ntohl, htonl, unsigned long int, (x),
	       DEFUN(htonl, (x), unsigned long int))

function_alias(ntohs, htons, unsigned short int, (x),
	       DEFUN(htons, (x), unsigned short int))
