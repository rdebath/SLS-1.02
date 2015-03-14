#ifdef DEBUG
if (debuglevel & 2560)
{
	int             j;

	if (debuglevel & 512 && (tracen > 0 && traceflag))
	{
	traceline[0]='\0';
	for (j=1;tracelog[j];j++){
		algbr(tracelog[j]>>8,tracelog[j]&0xff,0);
		strcat(traceline," ");
		strcat(traceline,mvstr[0]);
	}

	printf("Ply %d alpha %d beta %d score %d %s\n", ply, alpha, beta, score,traceline);
	if(debuglevel & 2048){
		for (j = ply; j < ply + 1; j++)
		{
			int             idb;

			for (idb = TrPnt[j]; idb < TrPnt[j + 1]; idb++)
			{
				algbr(Tree[idb].f, Tree[idb].t, Tree[idb].flags);
				printf("level 512 %d-->%d %s %d %d %x\n", ply, idb, mvstr[0], Tree[idb].score, Tree[idb].width, Tree[idb].flags);
			}
		}
}
}
	}

#endif
