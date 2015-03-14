/*
 *	hosts.h Copyright (C) 1992 Drew Eckhardt 
 *	mid to low-level SCSI driver interface header by	
 *		Drew Eckhardt 
 *
 *	<drew@colorado.edu>
 *
 *       Modified by Eric Youngdale eric@tantalus.nrl.navy.mil to
 *       add scatter-gather, multiple outstanding request, and other
 *       enhancements.
 */

#ifndef _HOSTS_H
	#define _HOSTS_H

/*
	$Header: /usr/src/linux/kernel/blk_drv/scsi/RCS/hosts.h,v 1.1 1992/07/24 06:27:38 root Exp root $
*/

/*
	The Scsi_Host type has all that is needed to interface with a SCSI
	host in a device independant matter.  
*/

#define SG_NONE 0
#define SG_ALL 0xff

/* The various choices mean:
   NONE: Self evident.  Host adapter is not capable of scatter-gather.
   ALL:  Means that the host adapter module can do scatter-gather,
         and that there is no limit to the size of the table to which
	 we scatter/gather data.
  Anything else:  Indicates the maximum number of chains that can be
        used in one scatter-gather request.
*/

typedef struct     
	{
	/*
		The name pointer is a pointer to the name of the SCSI
		device detected.
	*/

	char *name;

	/*
		The detect function shall return non zero on detection,
		and initialize all data necessary for this particular
		SCSI driver.  It is passed the host number, so this host
		knows where it is in the hosts array
	*/

	int (* detect)(int); 

	/*
		The info function will return whatever useful
		information the developer sees fit.              
	*/

        const char *(* info)(void);

	/*
		The command function takes a target, a command (this is a SCSI 
		command formatted as per the SCSI spec, nothing strange), a 
		data buffer pointer, and data buffer length pointer.  The return
		is a status int, bit fielded as follows : 
		Byte	What
		0	SCSI status code
		1	SCSI 1 byte message
		2 	host error return.
		3	mid level error return
	*/

	int (* command)(Scsi_Cmnd *);

        /*
                The QueueCommand function works in a similar manner
                to the command function.  It takes an additional parameter,
                void (* done)(int host, int code) which is passed the host 
		# and exit result when the command is complete.  
		Host number is the POSITION IN THE hosts array of THIS
		host adapter.
        */

        int (* queuecommand)(Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));

	/*
		Since the mid level driver handles time outs, etc, we want to 
		be able to abort the current command.  Abort returns 0 if the 
		abortion was successful.  If non-zero, the code passed to it 
		will be used as the return code, otherwise 
		DID_ABORT  should be returned.

		Note that the scsi driver should "clean up" after itself, 
		resetting the bus, etc.  if necessary. 
	*/

	int (* abort)(Scsi_Cmnd *, int);

	/*
		The reset function will reset the SCSI bus.  Any executing 
		commands should fail with a DID_RESET in the host byte.
	*/ 

	int (* reset)(void);
	/*
		This function is used to select synchronous communications,
		which will result in a higher data throughput.  Not implemented
		yet.
	*/ 

	int (* slave_attach)(int, int);
	/*
		This function determines the bios parameters for a given
		harddisk.  These tend to be numbers that are made up by
		the host adapter.  Parameters:
		size, device number, list (heads, sectors, cylinders)
	*/ 

	int (* bios_param)(int, int, int []);
	
	/*
		This determines if we will use a non-interrupt driven
		or an interrupt driven scheme,  It is set to the maximum number
		of simulataneous commands a given host adapter will accept.
	*/
	int can_queue;

	/*
		In many instances, especially where disconnect / reconnect are 
		supported, our host also has an ID on the SCSI bus.  If this is 
		the case, then it must be reserved.  Please set this_id to -1 if
 		your settup is in single initiator mode, and the host lacks an 
		ID.
	*/
	
	int this_id;

	/*
	        This determines the degree to which the host adapter is capable
		of scatter-gather.
	*/

	short unsigned int sg_tablesize;

	/*
	  True if this host adapter can make good use of linked commands.
	  This will allow more than one command to be queued to a given
	  unit on a given host.  Set this to the maximum number of command
	  blocks to be provided for each device.  Set this to 1 for one
	  command block per lun, 2 for two, etc.  Do not set this to 0.
	  You should make sure that the host adapter will do the right thing
	  before you try setting this above 1.
	 */

	short cmd_per_lun;
	/*
		present contains a flag as to weather we are present -
		so we don't have to call detect multiple times.
	*/

	unsigned present:1;	
	/*
	  true if this host adapter uses unchecked DMA onto an ISA bus.
	*/
	unsigned unchecked_isa_dma:1;
	} Scsi_Host;

/*
	The scsi_hosts array is	the array containing the data for all 
	possible <supported> scsi hosts.   
*/

extern Scsi_Host scsi_hosts[];

/*
	This is our semaphore array, used by scsi.c, sd.c, st.c.
	Other routines SHOULD NOT mess with it.  Your driver should NOT mess with it.
	This is used to protect against contention by disk and tape drivers.
*/

extern volatile unsigned char host_busy[];

/*
	This is the queue of currently pending commands for a given
	SCSI host.
*/

extern Scsi_Cmnd *host_queue[];

extern struct wait_queue *host_wait[];   /* For waiting until host available*/

/*
	scsi_init initializes the scsi hosts.
*/


void scsi_init(void);

#define BLANK_HOST {"", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
#endif
