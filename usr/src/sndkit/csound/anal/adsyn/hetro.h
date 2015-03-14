#include <math.h>                                     /*   HETRO.H   */

#define TWOPI   6.28308530
#define	PI	3.14159265
#define	SQRTOF3	1.73205081
#define SQUELCH	0.5	/* % of max ampl below which delta_f is frozen */

/* lowest heterodyne freq = sr/bufsiz */

double 	sq();

float  	x1,x2,yA,y2,y3,			/* lpf coefficients*/
	cur_est,			/* current freq. est.*/
	freq_est = 0,			/* harmonic freq. est.*/
	fund_est,			/* fundamental est.*/
	t,				/* fundamental period est.*/
	delta_t,			/* sampling period*/
	sr=20000,			/* sampling rate*/
       	freq_c = 0,			/* filter cutoff freq.*/
	input_dur = 0,		        /* sample input duration*/
	beg_time = 0, total_time;	/* begin time and total analysis time*/

double 	get_val(),			/* get value from buffer*/
	*cos_mul, *sin_mul,             /* quad. term buffers*/
       	*a_term, *b_term,               /*real & imag. terms*/
        *r_ampl,		        /* pt. by pt. amplitude buffer*/
	*r_phase,		        /* pt. by pt. phase buffer*/
	*a_avg,			        /* output dev. freq. buffer*/
	new_ph,				/* new phase value*/
       	old_ph = 0,			/* previous phase value*/
       	jmp_ph = 0,			/* for phase unwrap*/
        *ph_av1, *ph_av2, *ph_av3,      /*tempor. buffers*/
        *amp_av1, *amp_av2, *amp_av3,   /* same for ampl.*/
	m_amp = 1000;       		/* maximum amplitude at output*/
       
long    n, start = 0,			/* # of pts. in one per. of sample*/
	*(MAGS[50]), *(FREQS[50]);      /* magnitude and freq. output buffers*/

int     maxamp = 0,			/* max. ampl. of analysis pts.*/
	harnum, maxhar = 50,		/* current harmonic # and max # */
	f_ext = 1;			/* adsyn file extension*/

short   *(A_TIME[50]), *(F_TIME[50]),
	num_pts = 256;

int     skip,				/* flag to stop analysis if zeros*/
	bufsiz = 1,		        /* circular buffer size*/
	desc,nrd,			/* file descriptors*/
	len;				/* sample file length*/

char    filnum[100],			/* sample file name*/
	*auxp,				/* pointer to sample file */
	message[128];
