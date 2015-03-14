/*//////////////////////////////////////////////////////////////////////*/
char *
strstr(s1,s2)
	char *s1,*s2;
{	char *p1;
	int len;

	len = strlen(s2);
	for( p1 = s1; *p1; p1 ++ )
		if( *p1 == *s2 && strncmp(p1,s2,len)==0 ) 
			return p1;
	return 0;
}
