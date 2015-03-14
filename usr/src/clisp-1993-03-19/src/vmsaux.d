# Hilfsfunktionen für CLISP auf VMS
# Zum Teil abgeschrieben von sysdep.c aus der Emacs-Distribution.
# Bruno Haible 11.3.1993

#include "lispbibl.c"

# ==============================================================================

# Allgemein:

# Event-Flags allgemein:
local int process_ef = 0;

void vms_init (void);
void vms_init()
  {
    if (!process_ef) { LIB$GET_EF(&process_ef); } # Event-Flag allozieren
    SYS$CLREF(process_ef); # Event-Flag löschen
    vms_init_timer();
    vms_init_input();
  }

# ==============================================================================

# Event-Flags für den Timer:
local int timer_ef = 0;
local int timer_ef_list;

void vms_init_timer (void);
void vms_init_timer()
  {
    if (!timer_ef) { LIB$GET_EF(&timer_ef); } # Event-Flag allozieren
    SYS$CLREF(timer_ef); # Event-Flag löschen
    timer_ef_list = bit(timer_ef%32) | bit(process_ef%32);
  }

# Wartet amount/100 sec.
# The standard `sleep' routine works some other way and it stops working
# if you have ever quit out of it. This one continues to work.
global void vms_sleep (int amount);
global void vms_sleep(amount)
  var int amount;
  { var uintL time[2];
    # Convert to VMS format: time := amount * (1/100 s) / (100 ns)
    # (negativ, bedeutet delta time)
    local var sintL zero = 0;
    local var sintL large = -10000000/ticks_per_second;
    LIB$EMUL(&amount,&large,&zero,&time);
    # Timer-Hackerei:
    SYS$CANTIM(1,0); # request 1 löschen
    if (SYS$SETIMR(timer_ef,time,0,1) & 1) # Set timer, request 1
      { SYS$WAITFR(timer_ef); } # Wait for timer expiry only
  }

# ==============================================================================

# ttyname(), siehe TTYNAME(3V)
global char* ttyname (int fd);
global char* ttyname(fd)
  var int fd;
  { local var char buffer[MAXPATHLEN];
    getname(fd,&buffer);
    return &buffer;
  }

# ==============================================================================

# Directories lesen:

# include <errno.h>
# include <setjmp.h>
#ifndef FAB$C_BID
  #include <fab.h>     # FAB = File Access Block
#endif
#ifndef NAM$C_BID
  #include <nam.h>
#endif
# #ifndef RMS$_SUC
#   #include <rmsdef.h>  # RMS = Record Management Service
# #endif

# Filename muß bereits die Gestalt "host::device:[directory]*.*;*" haben.
global DIR* opendir (char* filename);
global struct direct * readdir (DIR* dirp);
global void closedir (DIR* dirp);

local DIR search_dir;

local jmp_buf search_jmpbuf;

local struct direct * * dpp;

local void nextfile_ok (struct FAB * fabptr);
local void nextfile_error (struct FAB * fabptr);

global DIR* opendir(filename)
  var reg5 char* filename;
  { var reg4 DIR* dirp = &search_dir;
    # Das ganze Directory auf einmal einlesen:
    dirp->dd_all = NULL;
    { var char search_esn[NAM$C_MAXRSS];
      var char search_rsn[NAM$C_MAXRSS];
      var struct NAM search_nam = cc$rms_nam;
      search_nam.nam$l_esa = search_esn; search_nam.nam$b_ess = sizeof(search_esn);
      search_nam.nam$l_rsa = search_rsn; search_nam.nam$b_rss = sizeof(search_rsn);
     {var struct FAB search_fab = cc$rms_fab;
      search_fab.fab$l_fna = filename; search_fab.fab$b_fns = asciz_length(filename);
      search_fab.fab$l_nam = &search_nam;
      search_fab.fab$l_fop = FAB$M_NAM;
      if (setjmp(search_jmpbuf)==0)
        { loop
            { var reg1 int status;
              # nächstes File suchen:
              status = LIB$FILE_SCAN(&search_fab,&nextfile_ok,&nextfile_error);
              if (status==0) # Keine weiteren Files?
                break;
              if (!(status&1)) # Error?
                { errno = EVMSERR; vaxc$errno = status; longjmp(search_jmpbuf,1); }
            }
          LIB$FILE_SCAN_END(&search_fab);
        }
        else
        # Wegen Error mit longjmp() herausgesprungen.
        { LIB$FILE_SCAN_END(&search_fab);
          if (!(errno==0)) { OS_error(); }
          # malloc() schaffte es nicht mehr.
          closedir(dirp); # Liste wieder freigeben
          return NULL;
        }
    }}
    dirp->dd_next = dirp->dd_all;
    return dirp;
  }

local void nextfile_ok (fabptr)
  var reg6 struct FAB * fabptr;
  { var reg5 struct NAM * namptr = fabptr->fab$l_nam;
    # Namen namptr->nam$l_rsa[0..namptr->nam$b_rsl-1]
    # (bzw. nur den Name/Typ/Version-Teil) herauskopieren:
    var reg3 uintC len = namptr->nam$b_rsl;
    #if 0 # den ganzen Namen
    var reg2 char* ptr1 = &namptr->nam$l_rsa[0];
    #else # nur den hinteren Teil
    var reg2 char* ptr1 = &namptr->nam$l_rsa[len];
    {var reg3 uintC count = len;
     len = 0;
     dotimesC(count,count,
       { var reg1 char* ch = ptr1[-1];
         if ((ch==':') || (ch==']') || (ch=='>')) break;
         ptr1--; len++;
       });
    }
    #endif
   {var reg4 struct direct * dp = malloc(offsetof(struct direct,d_name[0])+len);
    if (dp==NULL) { errno=0; longjmp(search_jmpbuf,1); }
    {var reg1 char* ptr2 = &dp->d_name[0];
     dp->d_namlen = len;
     dotimesC(len,len, { *ptr2++ = *ptr1++; } );
    }
    # und dp in die Liste einhängen:
    dp->d_next = NULL; *dpp = dp; dpp = &dp->d_next;
  }}

local void nextfile_error (fabptr)
  var struct FAB * fabptr;
  { # Man könnte hier errno und vaxc$errno ausgeben.
    # Man könnte auch ein longjmp(search_jmpbuf,1); ausführen.
    # Wir tun all das nicht, weil es vielleicht besser es, wegen eines
    # einzelnen Files nicht zu viel Unruhe zu stiften.
  }

global void closedir(dirp)
  var reg2 DIR* dirp;
  { # Wir müssen nur die Liste der malloc()-Blöcke freigeben:
    var reg1 struct direct * dp = dirp->dd_all;
    until (dp==NULL)
      { var reg2 struct direct * next = dp->d_next;
        free(dp);
        dp = next;
  }   }

global struct direct * readdir(dirp)
  var reg2 DIR* dirp;
  { var reg1 struct direct * next = dirp->dd_next;
    if (next == NULL) # Ende der Liste erreicht?
      { return NULL; }
      else
      { dirp->dd_next = next->d_next; return next; }
  }

# ==============================================================================

