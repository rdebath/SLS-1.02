/*****************************************************************************/
/*									     */
/* Program:  hetro.c							     */
/* Author:   Tom Sullivan MIT EMS  Nov 21, 1986				     */
/* Revised:  Tom Sullivan MIT EMS  March 16, 1987 (made more efficient)      */
/* Function: Fixed frequency heterodyne filter analysis.		     */
/* Comments/Complaints: sullivan@ems.media.mit.edu			     */
/*									     */
/* compile as : cc -o hetro hetro.c -lm                                      */
/*									     */
/*****************************************************************************/

/* INCLUDE DECLARATIONS */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <math.h>
#include "hetro.h"

/* MAIN PROGRAM */

short 	*adp; 		/*pointer to front of sample file*/
struct 	stat statbuf;	
char	*malloc();	/* allocate space */
double	*c_p,*s_p;	/* pointers to space for sine and cos terms */

main(argc,argv)
int argc;
char **argv;
{
	register int i;
	long     length;

	/* PROCESS INPUT ARGUMENTS */

	if (argc > 2) {
	  for (i=1; i<argc; i++) {
	    if (*(argv[i]++) == '-') {
	      switch (*(argv[i]++)) {
		case 'b':  if (sscanf(argv[i],"%f",&beg_time) < 1)
			     quit("Invalid beginning time");
			     break;
		case 'e':  if (sscanf(argv[i],"%d",&f_ext) < 1) 
			     quit("Invalid file extension");
			     break;
		case 'f':  if (sscanf(argv[i],"%f",&freq_c) < 1) 
			     quit("Invalid filter cutoff");
			     break;
		case 'h':  if (sscanf(argv[i],"%d",&maxhar) < 1) 
			     quit("Invalid harmonic count");
			     break;
		case 'i':  if (sscanf(argv[i],"%f",&input_dur) < 1) 
			     quit("Invalid duration time");
			     break;
		case 'm':  if (sscanf(argv[i],"%f",&m_amp) < 1) 
			     quit("Invalid amplitude maximum");
			     break;
		case 'n':  if (sscanf(argv[i],"%d",&num_pts) < 1) 
			     quit("Invalid number of output points");
			     break;
		case 's':  if (sscanf(argv[i],"%f",&sr) <1) 
			     quit("Invalid sampling rate");
			     break;
		default:   quit("Invalid switch option");
		}
	      }
	    else {
	      --argv[i];
	      goto label;
	      }
	    }

label:	
    	  if (sscanf(argv[i++],"%f",&fund_est) < 1) 
            quit("Bad fundamental argument");
	  else fprintf(stderr,"Est. fund. freq. = %f\n",fund_est);
  
  	  fprintf(stderr,"Input file:  %s\n",argv[i]);		
  	  if (sscanf(argv[i],"%s",filnum) < 1) 
    	    quit("Invalid serial #");

  	  fprintf(stderr,"Output file:  adsyn.%d\n",f_ext);

          if (freq_c > 1)
            fprintf (stderr,"Filter cutoff freq. = %f\n",freq_c);
    	}
        else quit("Too few arguments");

	while (bufsiz < (sr/fund_est + 0.5)) 
	  bufsiz *= 2;
				
	delta_t=1/sr;				/* calculate # of samples   */
	t=1/fund_est;				/* in one fundamental period*/
	n=floor((double)t/delta_t+0.5);		/* of the input waveform    */

	if ((input_dur < 0) || (beg_time < 0))
	  quit("input and begin times cannot be less than zero");
	if (beg_time != 0)
	  total_time = beg_time + input_dur;
	else total_time = input_dur;
		
	length = len = 0;
	lpinit();				/* calculate LPF coeffs. */

	if ((desc = open(filnum,0)) < 0) {	/* open sample file */
	  sprintf(message,"can't open %s",filnum);
          quit (message);
	  }

	fstat(desc,&statbuf);
	len = statbuf.st_size;
	if (len < sr*total_time*2)
	  quit("invalid choice of constraint times");
	
	if (input_dur == 0) {
	  length = len;
	  input_dur = length/(2 * sr);
	  }
	else length = (long)sr*input_dur*2;

	if ((num_pts > 32767) || (num_pts >= (sr*input_dur - n))) 
	  quit("number of output points is too great");

	auxp = malloc(length); 	/* allocate space for it*/
	fprintf(stderr,"file length to be analyzed = %d bytes\n",length);
	
	if (beg_time != 0) {
	  start = 2 * (long)(sr*beg_time);
	  lseek(desc,start,0);
	  }

	if((nrd = read(desc,auxp,length)) <= 0) {  /* and read it in */
	  sprintf(message,"Read error on %s\n",filnum);
	  quit(message);
	  }
	c_p = (double *) malloc(nrd<<2);	/* allocate space for the */
	s_p = (double *) malloc(nrd<<2);	/* quadrature terms */
	cos_mul = (double *) malloc(bufsiz<<3);
	sin_mul = (double *) malloc(bufsiz<<3);
	a_term = (double *) malloc(bufsiz<<3);
	b_term = (double *) malloc(bufsiz<<3);
	r_ampl = (double *) malloc(bufsiz<<3);
	ph_av1 = (double *) malloc(bufsiz<<3);
	ph_av2 = (double *) malloc(bufsiz<<3);
	ph_av3 = (double *) malloc(bufsiz<<3);
	r_phase = (double *) malloc(bufsiz<<3);
	amp_av1 = (double *) malloc(bufsiz<<3);
	amp_av2 = (double *) malloc(bufsiz<<3);
	amp_av3 = (double *) malloc(bufsiz<<3);
	a_avg = (double *) malloc(bufsiz<<3);

	for (i = 0; i < maxhar; i++) {
	  MAGS[i] = (long *)malloc(num_pts<<2);
	  FREQS[i] = (long *)malloc(num_pts<<2);
	  A_TIME[i] = (short *)malloc((num_pts + 1)<<1);
	  F_TIME[i] = (short *)malloc((num_pts + 1)<<1);
	  }
	adp = (short *)auxp;			/* set pointer to front of*/
	close(desc);				/* sample data block */
	for (harnum=0; harnum<maxhar; harnum++) { /* for requested harmonics*/
	  freq_est += fund_est;		  /* do analysis */
	  cur_est = freq_est;
	  for (i=0; i < bufsiz; i++) {   /* clear all buffers*/
	    cos_mul[i] = 0.;
            sin_mul[i] = 0.;
	    a_term[i] = 0.;
	    b_term[i] = 0.;
	    r_ampl[i] = 0.;
	    ph_av1[i] = 0.;
	    ph_av2[i] = 0.;
	    ph_av3[i] = 0.;
	    r_phase[i] = 0.;
	    amp_av1[i] = 0.;
	    amp_av2[i] = 0.;
	    amp_av3[i] = 0.;
	    a_avg[i] = 0.;
	    }
	  fprintf(stderr,"analyzing harmonic #%d\n",harnum);
	  fprintf (stderr,"harmonic frequency est. = %f\n", cur_est);
	  hetdyn();  /* perform actual computation */
	  }
	free(cos_mul);
	free(sin_mul);
	free(a_term);
	free(b_term);
	free(r_ampl);
	free(ph_av1);
	free(ph_av2);
	free(ph_av3);
	free(r_phase);
	free(amp_av1);
	free(amp_av2);
	free(amp_av3);
	free(a_avg);
	free(auxp);
	free(c_p);			/* free allocated space */
	free(s_p);
	filedump();			/* write output to adsyn file */
	for (i = 0; i < maxhar; i++) {
	  free(MAGS[i]);
	  free(FREQS[i]);
	  free(A_TIME[i]);
	  free(F_TIME[i]);
	  }
	exit(0);
}

/* HETERODYNE FILTER */


hetdyn() 

{

long 	sample;
double 	temp_a, temp_b, get_val(), out_a;
register double *cos_p,*sin_p,*cos_samp,*sin_samp;
int 	alpha, ta = -1;
register short *ptr;

	jmp_ph = 0;   			/* set initial phase to 0 */
	temp_a = temp_b = 0;
	cos_p = c_p;
	sin_p = s_p;
	for (sample=0;sample < (nrd/2); sample++){  /* do all quadrature calc*/
	  ptr = adp + sample;		   /* at once and point to it*/
	  *cos_p++ = (double)(*ptr)*cos(TWOPI*cur_est*sample*delta_t);
	  *sin_p++ = (double)(*ptr)*sin(TWOPI*cur_est*sample*delta_t);
	  }

	for (sample=0;sample < (input_dur*sr - n); sample++) {
	  cos_samp = c_p + sample;   
	  sin_samp = s_p + sample;   
	  if ((sample == 0) && ((nrd/2) >= n)) {/* for first sample */
	    for (alpha = 0; alpha < n; alpha++) {
	      temp_a += *cos_samp++; /* sum over # samples in one */
	      temp_b += *sin_samp++; /* period of fund. freq. */
	      }
	    }
	  else {
			/* if more than 1 fund. per. away from file end */
			/* remove front value and add on new rear value */
			/* to obtain summation term for new sample! */
	    if (sample <= ((nrd/2) - n)) {		
	      temp_a += (*(cos_samp + n - 1) - *(cos_samp - 1));
	      temp_b += (*(sin_samp + n - 1) - *(sin_samp - 1));
	      }
	    else {      
	      skip = 1;
	      temp_a = temp_b = 0;
	      }
	    }
	  put_val(cos_mul,sample,temp_a);/* store values into buffers*/
	  put_val(sin_mul,sample,temp_b);
	  if ((freq_c <= 1) || (sample < 3)) {
	    average(n,cos_mul,a_term,sample);/* average over previous */
	    average(n,sin_mul,b_term,sample);/* values 1 fund. per. ago*/
	    }
	  else {
	    lowpass(a_term,cos_mul,sample);
	    lowpass(b_term,sin_mul,sample);
	    }
	  output_ph(sample);  /*calculate mag. and phase for sample*/
	  out_a = (sample*num_pts/(input_dur*sr - n)); /* choose evenly */
	  if ((int)out_a > ta){		       /* spaced samples in*/
	    output(sample,(int)out_a);     /* time to write to */
	    ta = (int)out_a;	       /* output file  */
	    }
	  if (skip){
	    skip = 0;  /* quit if no more samples in file */
	    break;
	    }
	  }
}

/* lowpass coefficient ititializer */


lpinit() 
{	/* 3rd order butterworth LPF coefficients calculated using */
		/* impulse invariance */
float costerm,sinterm;
double omega_c;

        omega_c=freq_c*TWOPI;
	costerm=cos(SQRTOF3*omega_c*delta_t/2);
	sinterm=sin(SQRTOF3*omega_c*delta_t/2);
	x1=omega_c*delta_t*(exp(-omega_c*delta_t) + exp(-omega_c*delta_t/2)
	    *(-costerm + sinterm/SQRTOF3));
	x2=omega_c*delta_t*(exp(-omega_c*delta_t) - exp(-3*omega_c*delta_t/2)
	    *(costerm + sinterm/SQRTOF3));
	yA=(-(exp(-omega_c*delta_t) + 2*exp(-omega_c*delta_t/2)*costerm));
	y2=2*exp(-3*omega_c*delta_t/2)*costerm + exp(-omega_c*delta_t);
	y3=(-exp(-2*omega_c*delta_t));
}

/* lowpass function
 * must be called with x1,x2,yA,y2,y3  
 * initialized
 */


lowpass(out,in,smpl)

long smpl;		/* calls LPF function */
double *in, *out; 

{
double get_val();
	
	put_val(out,smpl,(x1*
	    get_val(in,smpl-1) + x2*get_val(in,smpl-2) -
	    yA*get_val(out,smpl-1) - y2*
	    get_val(out,smpl-2) - y3*get_val(out,smpl-3)));
}

/* AVERAGES OVER 'WINDOW' SAMPLES */


average(window,in,out,smpl) 
			     /* this is actually a comb filter with 'Z' */
long window,smpl;	     /* transform of (1/w *[1 - Z**-w]/[1 - Z**-1]) */
register double *in,*out;    /* ie. zeros at all harmonic frequencies except*/
			     /* the current one where the pole cancels it */
{			     

double get_val();

	put_val(out,smpl,(double)(get_val(out,smpl-1)
	    +(1/(double)window)*(get_val(in,smpl)
	    -get_val(in,smpl-window))));
}



/* places value in array n at position smpl */


put_val(outb,smpl,value)

long smpl;
register double *outb, value; 

{
register int c;

	c = smpl;
	*(outb + ((c + bufsiz/2) & (bufsiz - 1))) = value;
}


/* returns value at position smpl in array n */


double get_val(inb,smpl)

register double *inb;
long smpl; 

{
register int c;

	c = smpl;
	return(*(inb + ((c + bufsiz/2) & (bufsiz - 1))));
}


/* output one freq_mag pair */


output(smpl,inc)

long smpl;
int inc; 			
				/* when called, gets frequency change */
{				/* and adds it to current freq. stores*/
				/* current amp and new freq in arrays */
double delt_freq;		
double get_val();

	delt_freq = get_val(a_avg,smpl); /* 0.5 for rounding*/
	FREQS[harnum][inc + 1] = (long)(delt_freq +((double) cur_est) + 0.5);
	MAGS[harnum][inc + 1]=(long) get_val(r_ampl,smpl);
	/*printf("F=%d\tA=%d\t%f\n",FREQS[harnum][inc + 1],MAGS[harnum][inc + 1],
		delt_freq);*/
}


/* update phase counter */


output_ph(smpl)			/* calculates magnitude and phase components */
long smpl; 			/* for each samples quadrature components, & */
				/* and unwraps the phase.  A phase difference*/
{				/* is taken to represent the freq. change.   */
double 	delt_temp;	        /* the pairs are then comb filtered.	     */
double 	temp_a,get_val();	

	if ((temp_a=get_val(a_term,smpl)) == 0)
	  new_ph=(-PI/2)*sgn(get_val(b_term,smpl));
	else new_ph= -atan((double)
	    	get_val(b_term,smpl)/temp_a) - PI*u(-temp_a);

	if (fabs((double)new_ph - old_ph)>PI) 
	  jmp_ph -= TWOPI*sgn(temp_a);

	old_ph = new_ph;
	put_val(r_phase,smpl,old_ph+jmp_ph);
	delt_temp = ((get_val(r_phase,smpl) - get_val(r_phase,smpl-1))/
		      (TWOPI*delta_t));
	if ((freq_c <= 1) || (smpl < 3)) {
	  put_val(amp_av1,smpl,(float)sqrt(sq(get_val(a_term,smpl))
	    	  + sq(get_val(b_term,smpl))));
	  average(n,amp_av1,amp_av2,smpl);
	  average(n,amp_av2,amp_av3,smpl);
	  average(n,amp_av3,r_ampl,smpl);
	  put_val(ph_av1,smpl,delt_temp);
	  average(n,ph_av1,ph_av2,smpl);
	  average(n,ph_av2,ph_av3,smpl);
	  average(n,ph_av3,a_avg,smpl);
	  }
	else {
	  put_val(r_ampl,smpl,(float)sqrt(sq(get_val(a_term,smpl))
	    	  + sq(get_val(b_term,smpl))));
	  put_val(a_avg,smpl,delt_temp);
	  }
}


/* RETURN SIGN OF ARGUMENT */

sgn(num)

double num; 

{
	if (num >= 0) return(1);
	return(-1);
}

/* UNIT STEP FUNCTION */

u(num)

double num; 

{
	if (num > 0) return(1);
	return(0);
}

/* RETURNS SQUARE OF ARGUMENT */

double sq(num)

double num; 

{
	return(num*num);
}	

quit (msg)

char	*msg;

{
	fprintf(stderr,"hetro:  %s\n",msg);
	exit(0);
}

/* WRITE OUTPUT FILE */
/* write "adsyn.f_ext" in dump format */


filedump() 

{
int 	inc, d;
double 	scale,x,y;
char    filename[10];
short   *(mags[50]), *(freqs[50]), *ptr;

	for (inc = 0; inc < maxhar; inc++) {
	  mags[inc] = (short *)malloc((num_pts + 2)<<1);
	  freqs[inc] = (short *)malloc((num_pts + 2)<<1);
	  }

	sprintf (filename,"adsyn.%d",f_ext);
	if ((d = creat(filename,0644)) < 0)
	  quit("cannot create output file\n");
		
	for (harnum=0; harnum<maxhar; harnum++) {
	  for (inc=1; inc < num_pts+1; inc++) {
	    if (MAGS[harnum][inc] > maxamp ||
	      MAGS[harnum][inc]<-maxamp) maxamp=MAGS[harnum][inc];
	    }
	  }

	scale=(double)(m_amp/(maxamp+1));
	fprintf(stderr,"scale=%f\n",scale);

	for (harnum = 0; harnum < maxhar; harnum++) {
	  for (inc = 0; inc < num_pts+1; inc++) {
	    x = (MAGS[harnum][inc]*scale);
	    mags[harnum][inc] =(short)(x*u(x));
	    y = FREQS[harnum][inc];
	    freqs[harnum][inc]= (short)(y*u(y)); 
	    }
	  }

	for (harnum=0; harnum < maxhar; harnum++) {
	  mags[harnum][0] = -1;
	  mags[harnum][num_pts+1] = mags[harnum][num_pts];
	  freqs[harnum][0] = -2;
	  freqs[harnum][num_pts+1] = freqs[harnum][num_pts];
	  A_TIME[harnum][num_pts] = 32767;
	  F_TIME[harnum][num_pts] = 32767;
	  for (inc = 0; inc < num_pts; inc++){
	    A_TIME[harnum][inc] = (short)(1000*inc*input_dur/num_pts);
	    F_TIME[harnum][inc] = A_TIME[harnum][inc];
	    }
	  }
	
	ptr = (short *)malloc(((2 * num_pts) + 3) * 2);
	for (harnum = 0; harnum < maxhar; harnum++) {
	  for (inc=0; inc < num_pts+2; inc++){
	    if (inc == 0)
	      *ptr = mags[harnum][inc];
	    else {
	      *(ptr + ((2 * inc) - 1)) = A_TIME[harnum][inc - 1];
	      *(ptr + (2 * inc)) = mags[harnum][inc];
	      }
	    }
	  write(d,ptr,(((2 * num_pts) + 3) * 2));	  
	  for (inc=0; inc < num_pts+2; inc++){
	    if (inc == 0)
	      *ptr = freqs[harnum][inc];
	    else {
	      *(ptr + ((2 * inc) - 1)) = F_TIME[harnum][inc - 1];
	      *(ptr + (2 * inc)) = freqs[harnum][inc];
	      }
	    }
	  write(d,ptr,(((2 * num_pts) + 3) * 2));	  
    	  }
	fprintf(stderr,"file dumped\n");
	close(d);
	free(ptr);
	for (inc = 0; inc < maxhar; inc++) {
	  free(mags[inc]);
	  free(freqs[inc]);
	  }
}
