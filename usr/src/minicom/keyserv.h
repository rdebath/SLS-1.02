/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 *
 * Messages to send to our "keyserv" program.
 */
#define KSTOP		1
#define KKPST		2
#define KVT100		3
#define KANSI		4
#define KMINIX		5
#define KSTART		6
#define KKPAPP		7
#define KCURST  	8
#define KCURAPP		9
#define KSIGIO		10
#define KSETESC		11
#define KSETBS		12

#ifdef _COHERENT
#  define SIGUSR1 SIGDIVE
#  define SIGUSR2 SIGOVFL
#endif
#define HELLO		SIGUSR1
#define ACK		SIGUSR2

#define KINSTALL 	100
#define KUNINSTALL	101
