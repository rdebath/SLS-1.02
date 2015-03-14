/* the small file that is used to generate an accurate vibrato table */
#include <math.h>
#include <stdio.h>

main()
	{
	int i;
	int amplitude = 512;
	int period = 64;
	double th;
	double pi;

	pi = 4.0 * atan(1.0);

	for (i = 0; i < period; i++) 
		{
		th = 2.0 * pi * i / (double)period;
		printf("%d,", (int) (0.5 + sin(th) * (double)amplitude));
		}
	}


	
