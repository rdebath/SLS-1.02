
#  This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for
#  details. If they are missing then this copy is in violation of 
#  the copyright conditions.           


BEGIN	{
	    print  "/*"
	    print  " *	comp_captab.c -- The names of the capabilities in a form ready for"
	    print  " *		         the making of a hash table for the compiler."
	    print  " *"
	    print  " */"
	    print  ""
	    print  ""
	    print  "#include \"compiler.h\""
	    print  "#include \"terminfo.h\""
	    print  ""
	    print  ""
	    print  "struct name_table_entry	cap_table[] ="
	    print  "{"
	}


$3 == "bool"	{
		    printf "\t{ 0,%15s,\tBOOLEAN,\t%3d },\n", $2, BoolCount++
		}


$3 == "num"	{
		    printf "\t{ 0,%15s,\tNUMBER,\t\t%3d },\n", $2, NumCount++
		}


$3 == "str"	{
		    printf "\t{ 0,%15s,\tSTRING,\t\t%3d }, \n", $2, StrCount++
		}


END	{
	    print  "};"
	    print  ""
	    printf "struct name_table_entry *cap_hash_table[%d];\n",\
					(BoolCount + NumCount + StrCount) * 2
	    print  ""
	    printf "int	Hashtabsize = %d;\n",\
					(BoolCount + NumCount + StrCount) * 2
	    printf "int	Captabsize = %d;\n", BoolCount + NumCount + StrCount
	    print  ""
	    print  ""
	    printf "#if (BOOLCOUNT!=%d)||(NUMCOUNT!=%d)||(STRCOUNT!=%d)\n",\
						BoolCount, NumCount, StrCount
	    print  "#error	--> terminfo.h and comp_captab.c disagree about the <--"
	    print  "#error	--> numbers of booleans, numbers and/or strings <--"
	    print  "#endif"
	}
