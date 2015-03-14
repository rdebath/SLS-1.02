/* roundup.c - emulate BSD roundup - rick sladkey */

int roundup(int num, int units)
{
	return (num + units - 1)/units*units;
}

