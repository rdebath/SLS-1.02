/* this defines a macro, as a supplement to sfheader.h */

#define newrwopensf(name,fd,sfh,sfst,prog,result,code) \
if ((fd = open(name, code))  < 0) {  \
	result = -1;  \
} \
else if (rheader(fd,&sfh)){ \
	fprintf(stderr,"%s: cannot read header from %s\n",prog,name); \
	result = -1;  \
} \
else if (!ismagic(&sfh)){ \
	fprintf(stderr,"%s: %s not a bsd soundfile\n",prog,name); \
	result = -1;  \
} \
else if (stat(name,&sfst)){ \
	fprintf(stderr,"%s: cannot get status on %s\n",prog,name); \
	result = -1;  \
} \
else result = 0;
