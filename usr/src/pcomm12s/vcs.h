/*
 * Definitions to support the detection of video command sequences
 */

#define VCS_SIZE	25
#define NUM_VCS		9

#define HOME		0
#define CLR_EOL		1
#define CLR_EOS		2
#define CLEAR		3
#define MV_UP		4
#define MV_DOWN		5
#define MV_RIGHT	6
#define MV_LEFT		7
#define MV_DIRECT	8

#define YES		1
#define NO		0
#define MAYBE		(-1)
