#ifdef DEBUG
			traceflag = tracetmp;
                        if (debuglevel & 256 || ((debuglevel & 1024) && traceflag && (!traceply || ply<= traceply) )) {
                                int             i;
                                algbr(node->f, node->t, node->flags);
                                for (i = 1; i < ply; i++)
                                        printf("    ");
                                printf("%s S%d d%d p%d n%d s%d a%d b%d best%d x%d\n", mvstr[0], Sdepth, depth, ply, node->score, score, alpha, beta, best,xxxtmp);
#ifdef notdef
				if(strcmp(mvstr[0],"b6a5") == 0 && Sdepth == 3 && ply == 5 && alpha == 4){
				printf("score %d node %d best %d alpha %d beta %d\n", score,node->score,best,alpha,beta);
				gets(mvstr[0]);
				}
#endif
                        }
#endif
