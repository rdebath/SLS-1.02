#ifdef DEBUG4
  if (debuglevel & 8)
    {
      int j;

      for (j = 1; j < 2; j++)
	{
	  int idb;

	  for (idb = TrPnt[j]; idb < TrPnt[j + 1]; idb++)
	    {
	      algbr (Tree[idb].f, Tree[idb].t, Tree[idb].flags);
	      printf ("level 8 %d-->%d %s %d %d\n", j, idb, mvstr[0], Tree[idb].score, Tree[idb].width);
	    }
	}
    }
#endif
