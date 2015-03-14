/*//////////////////////////////////////////////////////////////////////*/
#include <ctype.h>

strcasecmp(a,b)
	register char *a,*b;
{	register char ac,bc;

	for(;;){
		ac = *a++;
		bc = *b++;

		if(ac == 0)
			if(bc == 0)
				return 0;
			else	return -1;
		else	if(bc == 0)
				return 1;
			else	if(ac != bc){
					if(islower(ac)) ac = toupper(ac);
					if(islower(bc)) bc = toupper(bc);
					if( ac != bc )
						return ac - bc;
				}
	}
}
