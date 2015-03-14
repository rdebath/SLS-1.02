#ifdef DEBUG41
void
debug41 (short int score, short unsigned int xxx[], char ch)
{
  register int i;
  FILE *D;
  int r, c, l;
  struct leaf *xnode;

  D = fopen ("/tmp/DEBUG", "a+");
  if (D == NULL)
    {
      perror ("opening D");
    }

  ElapsedTime (2);
  fprintf (D, "%2d%c %6d %4ld %8ld  ", Sdepth, ch, score, et / 100, NodeCnt);

  for (i = 1; xxx[i]; i++)
    {
      if ((i > 1) && (i % 8 == 1))
	fprintf (D, "\n                          ");
      algbr ((short) (xxx[i] >> 8), (short) (xxx[i] & 0xFF), false);
      fprintf (D, "%5s ", mvstr[0]);
    }
  fprintf (D, "\n");
  fclose (D);
}

#endif
