/*
 * Definitions for the xmodem stuff.
 */

#define MAX_ERRORS	10

#define SOH		1
#define STX		2
#define EOT		4
#define ACK		6
#define NAK		21
#define CAN		24
#define CTRLZ		26

#define NUM_INTERNAL	6
#define XMODEM		0
#define XMODEM_1k	1
#define MODEM7		2
#define YMODEM		3
#define YMODEM_G	4
#define XASCII		5
#define EXT_1		6
#define EXT_2		7
#define EXT_3		8
#define EXT_MANUAL	9

#define ABORT		(-1)
#define ERROR		(-2)
#define CANCEL		(-3)

#define CHECKSUM	0
#define CRC_CHECKSUM	1
#define CRC		2
#define NONE		3

#define DOWN_LOAD	0
#define UP_LOAD		1
