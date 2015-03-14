
#  This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for
#  details. If they are missing then this copy is in violation of  
#  the copyright conditions. 

BEGIN		{
		    print "/*"
		    print "**	terminfo.h -- Definition of struct term"
		    print "*/"
		    print ""
                    print "#ifndef _TERMINFO_H"
		    print "#define _TERMINFO_H"
		    print ""
		    print "#ifndef CURSES"
		    print "#    include \"curses.h\""
		    print "#endif"
		    print ""
		    print "#	define CUR cur_term->"
		    print ""
		    print ""
		}


$3 == "bool"	{
		    printf "#define %-30s CUR Booleans[%d]\n", $1, BoolCount++
		}

$3 == "num"	{
		    printf "#define %-30s CUR Numbers[%d]\n", $1, NumberCount++
		}

$3 == "str"	{
		    printf "#define %-30s CUR Strings[%d]\n", $1, StringCount++
		}


END		{
			print  ""
			print  ""
			print  "typedef struct term"
			print  "{"
			print  "   char	 *term_names;	/* offset in str_table of terminal names */"
			print  "   char	 *str_table;	/* pointer to string table */"
			print  "   short Filedes;	/* file description being written to */"
			print  "   SGTTY Ottyb,		/* original state of the terminal */"
			print  "	 Nttyb;		/* current state of the terminal */"
			print  ""
			printf "   char		 Booleans[%d];\n", BoolCount
			printf "   short	 Numbers[%d];\n", NumberCount
			printf "   char		 *Strings[%d];\n", StringCount
			print  "} TERMINAL;"
			print  ""
			print  "TERMINAL	*cur_term;"
			print  ""
			printf "#define BOOLCOUNT %d\n", BoolCount
			printf "#define NUMCOUNT  %d\n", NumberCount
			printf "#define STRCOUNT  %d\n", StringCount
                        print ""
                        print "extern int read_entry(char *, TERMINAL*);"
                        print "extern int must_swap();"
			print ""
                        print "#endif"
		}
