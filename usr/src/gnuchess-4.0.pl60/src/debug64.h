#ifdef DEBUG4
	  if (debuglevel & 64)
	    {
	      algbr (PV >> 8, PV & 0xff, 0);
	      printf ("-get-> d=%d s=%d p=%d a=%d b=%d %s\n", depth, score, ply, alpha, beta, mvstr[0]);
	    }
#endif
