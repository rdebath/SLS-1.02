
#  This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for
#  details. If they are missing then this copy is in violation of  
#  the copyright conditions. 

BEGIN		{
			print  "/*" > "boolnames"
			print  " *	names.c - Arrays of capability names and codes"  > "boolnames"
			print  " *" > "boolnames"
			print  " */" > "boolnames"
			print  "" > "boolnames"
			print  "#define NULL (char *)0" > "boolnames"
			print  "" > "boolnames"
			print  "char	*boolnames[] = {" > "boolnames"
			print  "char	*boolfnames[] = {" > "boolfnames"
			print  "char	*boolcodess[] = {" > "boolcodes"
			print  "char	*numnames[] = {" > "numnames"
			print  "char	*numfnames[] = {" > "numfnames"
			print  "char	*numcodes[] = {" > "numcodes"
			print  "char	*strnames[] = {" > "strnames"
			print  "char	*strfnames[] = {" > "strfnames"
			print  "char	*strcodes[] = {" > "strcodes"
		}

$3 == "bool"	{
			printf "\t\t%s,\n", $2 > "boolnames"
			printf "\t\t\"%s\",\n", $1 > "boolfnames"
			printf "\t\t%s,\n", $4 > "boolcodes"
		}

$3 == "num"	{
			printf "\t\t%s,\n", $2 > "numnames"
			printf "\t\t\"%s\",\n", $1 > "numfnames"
			printf "\t\t%s,\n", $4 > "numcodes"
		}

$3 == "str"	{
			printf "\t\t%s,\n", $2 > "strnames"
			printf "\t\t\"%s\",\n", $1 > "strfnames"
			printf "\t\t%s,\n", $4 > "strcodes"
		}

END		{
			print  "\t\tNULL," > "boolnames"
			print  "};" > "boolnames"
			print  "" > "boolnames"
			print  "\t\tNULL," > "boolfnames"
			print  "};" > "boolfnames"
			print  "" > "boolfnames"
			print  "\t\tNULL," > "boolcodes"
			print  "};" > "boolcodes"
			print  "" > "boolcodes"
			print  "\t\tNULL," > "numnames"
			print  "};" > "numnames"
			print  "" > "numnames"
			print  "\t\tNULL," > "numfnames"
			print  "};" > "numfnames"
			print  "" > "numfnames"
			print  "\t\tNULL," > "numcodes"
			print  "};" > "numcodes"
			print  "" > "numcodes"
			print  "\t\tNULL," > "strnames"
			print  "};" > "strnames"
			print  "" > "strnames"
			print  "\t\tNULL," > "strfnames"
			print  "};" > "strfnames"
			print  "" > "strfnames"
			print  "\t\tNULL," > "strcodes"
			print  "};" > "strcodes"
		}
