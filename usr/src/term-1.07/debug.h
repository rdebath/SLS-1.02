#define WARNING \
    (!(write_noise)) ? 0 : fprintf

#define DEBUG_STATE \
    (!(debug & 1)) ? 0 : fprintf

#define DEBUG_MAIN \
     (!(debug & 2)) ? 0 : fprintf

#define DEBUG_SER \
      (!(debug &8)) ? 0 :fprintf


#define DEBUG_CHECK \
       (!(debug & 16)) ? 0 : fprintf

#define DEBUG_PED \
	(!(debug & 32)) ? 0 : fprintf

#define DEBUG_FP (!(debug & 64)) ? 0 : fprintf

#define DEBUG_LINK \
	 (!(debug & 128)) ? 0 : fprintf

#define DEBUG_LL \
         (!(debug & 256)) ? 0 : fprintf

#define DEBUG_C \
         (!(debug & 512)) ? 0 : fprintf

#define DEBUG_SEV \
         (!(debug & 1024)) ? 0 : fprintf
