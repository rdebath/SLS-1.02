BEGIN {
	print "CC	= cc"
	print ""
	print "                # C libraries"
	print "CLIBS	="
	print ""
	print "                # options for cc"
	print "CFLAGS	="
	print ""
	print "                # options for mtc"
	print "M2FLAGS	="
	print ""
	print "SYSTEM_	= SYSTEM_.o"
	print ""
	print "all	:"
	print ""
	print "# HEAD"
      }
/CONFIRM/ { conf [$2] = 1; }
/DEFINITION/ {
	 name = $3
	 extension = ".h"
	 print ""
	 print "sources	: " name ".h"
      }
/FOREIGN/ {
	 name = $3
	 extension = ".h"
	 print ""
	 print "sources	: " name ".h"
      }
/IMPLEMENTATION/ {
	 name = $3
	 extension = ".o"
	 print ""
	 print "sources	: " name ".c"
	 print name extension "	: " name ".c " name ".h"
	 cnt [name] ++
	 dep [name cnt [name]] = name extension
	 atom [name cnt [name]] = 1
      }
/PROGRAM/ {
	 name = $3
	 extension = ".o"
	 print ""
	 print "sources	: " name ".c"
	 print "all	: " name
	 print name extension "	: " name ".c"
	 progcnt ++
	 prog [progcnt] = name
	 cnt [name] ++
	 dep [name cnt [name]] = name extension
	 atom [name cnt [name]] = 1
      }
/IMPORT/ {
	 if (conf [$2] == 1) {
	    print name extension "	: " $2 ".h"
	    cnt [name] ++
	    dep [name cnt [name]] = $2
	 }
      }
END {
	print ""
	print "SYSTEM_.o	: SYSTEM_.c SYSTEM_.h"

	for (i = 1; i <= progcnt; i ++)
	{
	   program = prog [i]
	   programs = programs " " program
	   stkptr = 1
	   stack [stkptr] = program
	   while (stkptr > 0)
	   {
	      module = stack [stkptr]
	      stkptr --
	      set [module] = 1
	      setcnt ++
	      element [setcnt] = module
	      for (j = 1; j <= cnt [module]; j ++)
	      {
		 depmod = dep [module j]
		 if (set [depmod] == 0)
		 {
		    if (atom [module j] == 1)
		       object [++ objcnt] = depmod
		    stkptr ++
		    stack [stkptr] = depmod
		 }
	      }
	   }
	   print ""
	   print program "	: $(SYSTEM)	\\"
	   for (j = 1; j <= objcnt; j ++)
	      print "	" object [j] "	\\"
	   print ";	$(CC) $(CFLAGS) -o " program " $(SYSTEM)	\\"
	   for (j = 1; j <= objcnt; j ++)
	      print "	" object [j] "	\\"
	   print "	$(CLIBS)"
	   for (j = 1; j <= setcnt; j ++)
	      set [element [j]] = 0
	   setcnt = 0
	   objcnt = 0
	}

	print ""
	print "# TAIL"
	print ""
	print "clean	:"
	print "	rm -f core *.o"
	print ""
	print ".SUFFIXES: .md .mi .h .c .o"
	print ""
	print ".mi.c:"
	print "	mtc $(M2FLAGS) $*.mi;"
	print ""
	print ".md.h:"
	print "	mtc $(M2FLAGS) $*.md;"
	print ""
	print ".c.o:"
	print "	$(CC) $(CFLAGS) -c $*.c"
      }
