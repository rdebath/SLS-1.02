	if (1)
	  {
	    FILE *D;
	    int r, c, l;
	    extern unsigned short int PrVar[];
	    extern struct leaf *root;
	    D = fopen ("/tmp/DEBUGA", "a+");
	    fprintf (D, "score = %d\n", root->score);
	    fprintf (D, "inout move is %s\n", s);
	    for (r = 1; PrVar[r]; r++)
	      {
		algbr (PrVar[r] >> 8, PrVar[r] & 0xff, (short) 0);
		fprintf (D, " %s", mvstr[0]);
	      }
	    fprintf (D, "\n current board is\n");
	    for (r = 7; r >= 0; r--)
	      {
		for (c = 0; c <= 7; c++)
		  {
		    l = locn (r, c);
		    if (color[l] == neutral)
		      fprintf (D, " -");
		    else if (color[l] == white)
		      fprintf (D, " %c", qxx[board[l]]);
		    else
		      fprintf (D, " %c", pxx[board[l]]);
		  }
		fprintf (D, "\n");
	      }
	    fprintf (D, "\n");
	    fclose (D);
	  }
