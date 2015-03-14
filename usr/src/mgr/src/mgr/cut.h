#ifdef __STDC__
int paste(void);
int cut(void);
void zap_fhash(struct font *fnt);
#else
extern int paste();
extern int cut();
extern void zap_fhash();
#endif
/*{{{}}}*/
