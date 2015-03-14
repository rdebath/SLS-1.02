#!/bin/csh
#	time video scripts

set CAT=cat

foreach i ($*) 
	echo "$i"
	if (`basename $i .Z` != `basename $i`) then
		set CAT=zcat
		endif
	$CAT $i | to_ascii x | egrep "^T|^K" | awk -F" " '\
{\
	if ($1 == "K" && $2 == 0) {\
		sum += end - begin\
		count++\
		printf "  part %d: %f sec, sum: %f\n", count, end-start, sum\
		new = 0\
		}\
	else if ($1 == "T") {\
		if (new = 1) {\
			end = $2\
			}\
		else {\
			begin = $2\
			new = 1\
			}\
		}\
}'
end
