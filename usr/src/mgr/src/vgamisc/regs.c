#include <dos.h>
union REGS inregs,outregs;

#define CRT_I   0x3D4   /* CRT Controller Index (mono: 0x3B4) */
#define ATT_IW  0x3C0   /* Attribute Controller Index & Data Write Register */
#define GRA_I   0x3CE   /* Graphics Controller Index */
#define SEQ_I   0x3C4   /* Sequencer Index */
#define PEL_IW  0x3C8   /* PEL Write Index */

/* VGA data register ports */
#define CRT_D   0x3D5   /* CRT Controller Data Register (mono: 0x3B5) */
#define ATT_R   0x3C1   /* Attribute Controller Data Read Register */
#define GRA_D   0x3CF   /* Graphics Controller Data Register */
#define SEQ_D   0x3C5   /* Sequencer Data Register */
#define MIS_R   0x3CC   /* Misc Output Read Register */
#define MIS_W   0x3C2   /* Misc Output Write Register */
#define IS1_R   0x3DA   /* Input Status Register 1 (mono: 0x3BA) */
#define PEL_D   0x3C9   /* PEL Data Register */

/* VGA indexes max counts */
#define CRT_C   24      /* 24 CRT Controller Registers */
#define ATT_C   21      /* 21 Attribute Controller Registers */
#define GRA_C   9       /* 9  Graphics Controller Registers */
#define SEQ_C   5       /* 5  Sequencer Registers */
#define MIS_C   1       /* 1  Misc Output Register */
 
/* VGA registers saving indexes */
#define CRT     0               /* CRT Controller Registers start */
#define ATT     CRT+CRT_C       /* Attribute Controller Registers start */
#define GRA     ATT+ATT_C       /* Graphics Controller Registers start */
#define SEQ     GRA+GRA_C       /* Sequencer Registers */
#define MIS     SEQ+SEQ_C       /* General Registers */
#define END     MIS+MIS_C       /* last */

#define port_out(a,b)   outp(b,a)
#define port_in(a)	inp(a)

static int set_regs(char regs[])
{
    int i;

    port_out(0x00,GRA_I); 
    port_out(0x00,GRA_D);  		/* set/reset                        */
  
    port_in(IS1_R);  	 		/* clear flip-flop                  */
    port_out(0x00,SEQ_I); 
    port_out(0x01,SEQ_D); 		/* synchronous reset on             */
  
    port_out(regs[MIS+0], MIS_W); 	/* update misc output register      */
  
    port_out(0x1, SEQ_I); 
    port_out(regs[SEQ+1], SEQ_D);  	/* update clocking mode             */
  
    for (i = 2; i < SEQ_C; i++) {	/* sequencer registers              */
        port_out(i, SEQ_I); 
        port_out(regs[SEQ+i], SEQ_D); 
    }

    port_out(0x11, CRT_I); 		  
    port_out(regs[CRT+0x11]&0x7F, CRT_D);   /* deprotect registers 0-7      */
  
    for (i = 0; i < CRT_C; i++) { 	/* CRT controller registers 	    */
        port_out(i, CRT_I); 
        port_out(regs[CRT+i], CRT_D); 
    }

    for (i = 0; i < GRA_C; i++) { 	/* graphics controller registers    */
        port_out(i, GRA_I); 
        port_out(regs[GRA+i], GRA_D); 
    }
     
    for (i = 0; i < ATT_C; i++) {       /* attribute controller registers   */
        port_in(IS1_R);          	/* reset flip-flop                  */
        port_out(i, ATT_IW);
        port_out(regs[ATT+i],ATT_IW);
    }

    port_out(0x00, SEQ_I); 
    port_out(0x03, SEQ_D);   		/* synchronous reset off            */
  
    return 0;
}

int disable_video()
{
    /* disable video */
    port_in(IS1_R); 		
    port_out(0x00, ATT_IW);	
}

int save_regs(text_regs)
	char text_regs[];
{
	int i;
   	    /* save text mode VGA registers */
    	    for (i = 0; i < CRT_C; i++) {
    	        port_out(i, CRT_I); 
                text_regs[CRT+i] = port_in(CRT_D); 
            }
    	    for (i = 0; i < ATT_C; i++) {
        	port_in(IS1_R);
        	port_out(i, ATT_IW); 
        	text_regs[ATT+i] = port_in(ATT_R); 
    	    }
    	    for (i = 0; i < GRA_C; i++) {
        	port_out(i, GRA_I); 
        	text_regs[GRA+i] = port_in(GRA_D); 
    	    }
   	    for (i = 0; i < SEQ_C; i++) {
        	port_out(i, SEQ_I); 
        	text_regs[SEQ+i] = port_in(SEQ_D); 
    	    }
    	    text_regs[MIS] = port_in(MIS_R); 
}

enable_video()
{
    port_in(IS1_R);
    port_out(0x20,ATT_IW);
}

main(argc,argv)
	int argc;
	char **argv;
{
    unsigned char save[60];
    int old;
    int i;
    if (argc != 2) {
    	printf("Usage: regs videomode\n");
    	exit(1);
    }
    sscanf(argv[1],"%x",&i);
    old = SetMode(i);
    disable_video();
    save_regs(save);
    set_regs(save);
    puts("/* Use REGS.H under MS-DOS to create this file */\n");
    for (i=0;i<60;i+=10)
       printf("0x%02x,0x%02x,0x%02x,0x%02x,0x%02x,0x%02x,0x%02x,0x%02x,0x%02x,0x%02x,\n",
    		save[i],save[i+1],save[i+2],save[i+3],save[i+4],
    		save[i+5],save[i+6],save[i+7],save[i+8],save[i+9]);
    enable_video();
    SetMode(old);
}
    
SetMode(x)
	int x;
{
	int y;

	inregs.h.ah = 0x0f;
	int86(0x10,&inregs,&outregs);
	y = outregs.h.al;

	inregs.h.ah = 0x00;
	inregs.h.al = x;
	int86(0x10,&inregs,&outregs);
	return(y);
}
	
