/*
 *	hosts.c Copyright (C) 1992 Drew Eckhardt 
 *	mid to lowlevel SCSI driver interface by
 *		Drew Eckhardt 
 *
 *	<drew@colorado.edu>
 */


/*
 *	This file contains the medium level SCSI
 *	host interface initialization, as well as the scsi_hosts array of SCSI
 *	hosts currently present in the system. 
 */

#include <linux/config.h>
#include "../blk.h"
#include <linux/kernel.h>
#include "scsi.h"

#ifndef NULL 
#define NULL 0L
#endif

#include "hosts.h"

#ifdef CONFIG_SCSI_AHA1542
#include "aha1542.h"
#endif

#ifdef CONFIG_SCSI_AHA1740
#include "aha1740.h"
#endif

#ifdef CONFIG_SCSI_FUTURE_DOMAIN
#include "fdomain.h"
#endif

#ifdef CONFIG_SCSI_SEAGATE
#include "seagate.h"
#endif

#ifdef CONFIG_SCSI_ULTRASTOR
#include "ultrastor.h"
#endif

#ifdef CONFIG_SCSI_7000FASST
#include "wd7000.h"
#endif

#ifdef CONFIG_SCSI_DEBUG
#include "scsi_debug.h"
#endif

/*
static const char RCSid[] = "$Header: /usr/src/linux/kernel/blk_drv/scsi/RCS/hosts.c,v 1.1 1992/07/24 06:27:38 root Exp root $";
*/

/*
 *	The scsi host entries should be in the order you wish the 
 *	cards to be detected.  A driver may appear more than once IFF
 *	it can deal with being detected (and therefore initialized) 
 *	with more than one simulatenous host number, can handle being
 *	rentrant, etc.
 *
 *	They may appear in any order, as each SCSI host  is told which host number it is
 *	during detection.
 */

/* This is a placeholder for controllers that are not configured into
   the system - we do this to ensure that the controller numbering is
   always consistent, no matter how the kernel is configured. */

#define NO_CONTROLLER {NULL, NULL, NULL, NULL, NULL, NULL, NULL, \
	        NULL, NULL, 0, 0, 0, 0, 0, 0}

/*
 *	When figure is run, we don't want to link to any object code.  Since 
 *	the macro for each host will contain function pointers, we cannot 
 *	use it and instead must use a "blank" that does no such 
 *	idiocy.
 */

Scsi_Host scsi_hosts[] =
	{
#ifdef CONFIG_SCSI_AHA1542
	AHA1542,
#endif
#ifdef CONFIG_SCSI_AHA1740
	AHA1740,
#endif
#ifdef CONFIG_SCSI_FUTURE_DOMAIN
	FDOMAIN_16X0,
#endif
#ifdef CONFIG_SCSI_SEAGATE
	SEAGATE_ST0X,
#endif
#ifdef CONFIG_SCSI_ULTRASTOR
	ULTRASTOR_14F,
#endif
#ifdef CONFIG_SCSI_7000FASST
	WD7000,
#endif
#ifdef CONFIG_SCSI_DEBUG
	SCSI_DEBUG,
#endif
	};

/*
 *	Our semaphores and timeout counters, where size depends on MAX_SCSI_HOSTS here. 
 */

volatile unsigned char host_busy[MAX_SCSI_HOSTS];
volatile int host_timeout[MAX_SCSI_HOSTS];
int last_reset[MAX_SCSI_HOSTS];
Scsi_Cmnd *host_queue[MAX_SCSI_HOSTS]; 
struct wait_queue *host_wait[MAX_SCSI_HOSTS] = {NULL,};   /* For waiting until host available*/
int max_scsi_hosts = MAX_SCSI_HOSTS;  /* This is used by scsi.c */

void scsi_init(void)
	{
	static int called = 0;
	int i, count;	
	if (!called)
		{
		called = 1;	
		for (count = i = 0; i < MAX_SCSI_HOSTS; ++i)
			{
/*
 * Initialize our semaphores.  -1 is interpreted to mean 
 * "inactive" - where as 0 will indicate a time out condition.
 */ 

			host_busy[i] = 0;
			host_queue[i] = NULL;	
			
			if ((scsi_hosts[i].detect) &&  (scsi_hosts[i].present = scsi_hosts[i].detect(i)))
				{		
				printk ("scsi%d : %s.\n",
				         count, scsi_hosts[i].name);
				printk ("%s", scsi_hosts[i].info());
				++count;
				}
			}
		printk ("scsi : %d hosts.\n", count);
		}

	}

#ifndef CONFIG_BLK_DEV_SD
unsigned long sd_init(unsigned long memory_start, unsigned long memory_end){
  return memory_start;
};
unsigned long sd_init1(unsigned long memory_start, unsigned long memory_end){
  return memory_start;
};
void sd_attach(Scsi_Device * SDp){
};
int NR_SD=-1;
int MAX_SD=0;
#endif


#ifndef CONFIG_BLK_DEV_SR
unsigned long sr_init(unsigned long memory_start, unsigned long memory_end){
  return memory_start;
};
unsigned long sr_init1(unsigned long memory_start, unsigned long memory_end){
  return memory_start;
};
void sr_attach(Scsi_Device * SDp){
};
int NR_SR=-1;
int MAX_SR=0;
#endif


#ifndef CONFIG_BLK_DEV_ST
unsigned long st_init(unsigned long memory_start, unsigned long memory_end){
  return memory_start;
};
unsigned long st_init1(unsigned long memory_start, unsigned long memory_end){
  return memory_start;
};
void st_attach(Scsi_Device * SDp){
};
int NR_ST=-1;
int MAX_ST=0;
#endif

