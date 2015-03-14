#define MAXRC 13

#if defined(hpux) || defined(__hpux)
#define NO_GETDTABSIZE
#define NO_FD_SET
#endif

#if defined(_IBMR2)
#define NO_FD_SET
#endif
