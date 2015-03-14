int dn_skipname(char *p, char *eom)
{
	FILE *ofp;

	ofp = fopen("/dev/console", "w");
	if (ofp) {
		fprintf(ofp, "dn_skipname: %s\n", p);
		fclose(ofp);
	}
	return 0;
}

