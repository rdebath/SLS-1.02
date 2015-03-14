/* exp_log.h */

extern int loguser;	/* shared by cmdLoguser, cmdExpect and cmdSpawn */

/* special version of log for non-null-terminated strings which */
/* never need printf-style formatting. */
#define logn(buf,length)  { \
			  if (logfile) fwrite(buf,1,length,logfile); \
			  if (debugfile) fwrite(buf,1,length,debugfile); \
			  }

char *printify();
#define dprintify(x)	((is_debugging || debugfile)?printify(x):0)
/* in circumstances where "debuglog(printify(...))" is written, call */
/* dprintify instead.  This will avoid doing any formatting that would */
/* occur before debuglog got control and decided not to do anything */
/* because (is_debugging || debugfile) was false. */

void Log();
void errorlog();
void exp_debuglog();
void nflog();
void nferrorlog();

extern FILE *debugfile;
extern FILE *logfile;
extern int logfile_all;
extern int loguser;

extern int is_debugging;	/* useful to know for avoid debug calls */
