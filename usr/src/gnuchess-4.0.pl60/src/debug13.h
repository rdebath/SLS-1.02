#ifdef DEBUG13
      if (flag.timeout && !background)
	{
	  FILE *D;
	  int r, c, l;
	  struct leaf *xnode;

	  D = fopen ("/tmp/DEBUG", "a+");
	  fprintf (D, " %d ply %d sco %d TC %d gl %d cnt %d\n",
		   Sdepth, plyscore, score, TCcount,
		   globalpnt, TrPnt[2] - TrPnt[1]);
	  for (i = 1; tmp[i]; i++)
	    {
	      algbr (tmp[i] >> 8, tmp[i] & 0xff, 0);
	      fprintf (D, "%s ", mvstr[0]);
	    }
	  fprintf (D, "\n");
	  for (i = 1; PrVar[i]; i++)
	    {
	      algbr (PrVar[i] >> 8, PrVar[i] & 0xff, 0);
	      fprintf (D, "%s ", mvstr[0]);
	    }
	  fprintf (D, "\n");
	  algbr (root->f, root->t, root->flags);
	  fprintf (D, "%s ", mvstr[0]);
	  fprintf (D, "\n");
	  fclose (D);
	}
#endif
