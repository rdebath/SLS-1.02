#include <stdio.h>
#include "ptrack.h"


float ptable(fmin,fmax,tphi,tpsi,tgamph,tgamps,freq,n)
float fmin,fmax,tphi[50][5][25],tpsi[50][6][25],tgamph[50][5];
float tgamps[50][6],freq[50];
int n;
{
	int i,j,k,np,nt;
	static float phi[5][25],psi[6][25],gamphi[5],gampsi[6];
	float omega,t,trigpo();

	for (i=0 ; i<50 ; ++i)
	{
		freq[i] = fmin + ((float) (i)) * (fmax - fmin )/50.;
		t = ( (float) (NYQ/10.) )/ freq[i];
		nt = (int) t;
		n = (nt < 5 ? nt : 5);
		np=n+1;
		omega = (freq[i]*(2.*PI))/ (SR/10.);
		trigpo(omega,phi,psi,gampsi,gamphi,n);
		for ( j = 0 ; j < n ; ++j)
		{
			for ( k= 0 ; k < MM ; ++k) 
				tphi[i][j][k] = phi[j][k];
			tgamph[i][j] = gamphi[j];
		}
		for (j=0 ; j<np ; ++j)
		{
			for (k=0 ; k < MM ; ++k)
				tpsi[i][j][k]=psi[j][k];
			tgamps[i][j]=gampsi[j];
		}
	}
	return;
}


float
trigpo(omega,phi,psi,gampsi,gamphi,n)
float omega,psi[6][25],phi[5][25];
float gamphi[5],gampsi[6];
int n;
{
	int j=0;
	int k,np;
	double alpha,beta,gamma;
	double wcos[25],wsin[25];
	double p,z,a,b,yy;
	double sin(),cos();

	np=n+1;
	for (k=0 ; k<MM ; ++k)
	{
		yy = omega* (float) (k);
		wcos[k]=cos(yy);
		wsin[k]=sin(yy);
	}
	beta=gamma= ZERO;
	for (k=0 ; k<MM ; ++k)
	{
		p=wsin[k];
		z=p*p;
		beta+=z*wcos[k];
		gamma+=z;
		phi[0][k]=p;
	}
	gamphi[0]=gamma;
	a=TWO * beta/gamma;
	alpha=beta=gamma=ZERO;
	for (k=0 ; k<MM ; ++k)
	{
		p= (TWO * wcos[k]-a) * phi[0][k];
		alpha += wcos[k] * p * phi[0][k];
		beta += wcos[k]* ( p * p ) ;
		gamma += ( p * p );
		phi[1][k] = p ;
	}
	gamphi[1]=gamma;
	a = TWO * beta/gamma;
	b = TWO *alpha/gamphi[0];
	for (j=2 ; j<n ; ++j)
	{
		alpha=beta=gamma = ZERO;
		for (k=0 ; k< MM ; ++k)
		{
			p = (TWO * wcos[k] - a ) * phi[j-1][k] - b * phi[j-2][k];
			alpha += wcos[k] * p * phi[j-1][k];
			beta += wcos[k] * (p * p);
			gamma += (p * p) ;
			phi[j][k] = p ;
		}
		gamphi[j] = gamma;
		a = TWO * beta/gamma;
		b = TWO *alpha/gamphi[j-1];
	}
	beta = ZERO ;
	gamma = (double) MM;
	for ( k=0; k < MM ; ++k)
	{
		beta += wcos[k];
		psi[0][k]= ONE;
	}
	gampsi[0]=gamma;
	a=beta/gamma;
	alpha=beta=gamma= ZERO;
	for ( k=0 ; k < MM ; ++k)
	{
		p = wcos[k]-a;
		alpha += wcos[k] * p*psi[0][k];
		beta += wcos[k] * ( p * p );
		gamma += (p * p);
		psi[1][k] = p ;
	}
	gampsi[1] = gamma ;
	a = TWO * beta / gamma ;
	b = TWO * alpha / gampsi[0];
	for (j=2 ; j<np ;++j)
	{
		alpha=beta=gamma= ZERO;
		for ( k=0; k < MM ; ++k)
		{

			p=(TWO * wcos[k]-a)* psi[j-1][k]-b*psi[j-2][k];
			alpha += wcos[k]*p*psi[j-1][k];
			beta += wcos[k]* (p*p);
			gamma += (p*p);
			psi[j][k] = p;
		}
		gampsi[j]=gamma;
		a=TWO*beta/gamma;
		b=TWO*alpha/gampsi[j-1];
	}
	return;
}


float 
getrms(sig)
short *sig;		/* from float */
{
	int step;
	float rms = ZERO ;
	double sqrt();


	for (step=0; step<= LSLICE-1; ++step)
		rms += sig[step]*sig[step];
	return( sqrt((double) (rms / (double) LSLICE) ) );
}


float 
getpch(sigf,tphi,tpsi,tgamph,tgamps,freq,n)
float tphi[50][5][25],tpsi[50][6][35],tgamph[50][5],tgamps[50][5];
short *sigf;
float *freq;
int n;
{
	int i, j,tenj;
	float s[50],g[25],h[25];
	float fm,xx,qsum,search();


        for (j=0 ;j < JMAX ; ++j)
	{
		tenj = 10 * (j+1) - 1 ;
		s[j]=sigf[tenj];
	}
	for (i=0 ; i<MM ; ++i)
	{
		g[i] = .5 * ( s[MM+i-1] - s[MM-i-1] );
		h[i] = .5 * ( s[MM+i-1] + s[MM-i-1] );
	}
	qsum = 0.;
	for (i=0 ; i<MM ; ++i)
		qsum += (g[i]*g[i]) + (h[i]*h[i]);
	xx=search(&fm,qsum,tphi,tpsi,tgamph,tgamps,g,h,freq,n);
	return(xx);
}


float search(fm,qsum,tphi,tpsi,tgamph,tgamps,g,h,freq,n)
float *fm,qsum;
float tphi[50][5][25],tpsi[50][6][25],tgamph[50][5];
float tgamps[50][6],g[],h[],freq[50];
int n;
{
	float funmin = 1.e10,filfun(); 
	float fun[50];
	float f1,f2,f3,x0,x1,x2,x3,a,b,c,ftemp;
	int istar,i;



	filfun(tphi,tgamph,tpsi,tgamps,fun,freq,g,h,qsum,n);
	istar =  -1;
	for(i=0 ; i<50 ; ++i)
	{
		ftemp=fun[i];
		if(ftemp < funmin)
		{
			funmin = ftemp;
			istar = i;
		}
	}
	if( istar < 0 )
	{
		fprintf(stderr,"ptrack: search: error in search\n");
		exit();
	}
	if( istar == 0 || istar == 49 )
	{
		*fm = fun[istar];
		return (freq[istar]);
	} else {
		x1 = freq[istar-1];
		f1 = fun[istar-1];
		x2 = freq[istar];
		f2 = fun[istar];
		x3 = freq[istar+1];
		f3 = fun[istar+1];
		a = f3/((x3-x1)*(x3-x2));
		b = f2/((x2-x1)*(x2-x3));
		c = f1/((x1-x2)*(x1-x3));
		x0 = .5*(a*(x1+x2)+b*(x1+x3)+c*(x2+x3))/(a+b+c);
		*fm = a*(x0-x1)*(x0-x2)+b*(x0-x1)*(x0-x3)+c*(x0-x2)*(x0-x3);
		return(x0);
	}
}


float 
filfun(tphi,tgamph,tpsi,tgamps,fun,freq,g,h,qsum,n)
/* Too many magic numbers */
float tphi[50][5][25],tpsi[50][6][25],tgamph[50][5];
float tgamps[50][6],*g,*h,*freq,*fun,qsum;
int n;
{
	float c,sum;
	int i,np,j,k;


        for(i=0 ; i < 50 ;++i)
	{
		n = (NYQ/10.)/freq[i];
		n = (n < 5) ? n : 5;

	/*	n = (int)((((float)(NYQ/10.))/freq[i]) < 5) ? n : 5; */
		np = n+1;
		sum = ZERO;
                for(j=0 ; j < n ; ++j)
		{
			c = ZERO;
                        for(k=0 ; k< MM ; ++k)  
				c += g[k]*tphi[i][j][k];
			sum += (c*c)/tgamph[i][j];
		}
		for (j=0 ; j<np ; ++j)
		{
			c = ZERO;
			for (k=0 ; k < MM ; ++k) 
				c += h[k] * tpsi[i][j][k];
			sum += (c*c)/tgamps[i][j];
		}
		fun[i] = qsum-sum;
	}
	return;
}


float lowpass(x)
short x;		/* from float */
{
	float c= .00048175311;
	float a1= -1.89919924;
	float c1= -1.923248041;
	float d1= .985720370;
	float a2= -1.86607670;
	float c2= -1.90075003;
	float d2= .948444690;
	float a3= -1.66423461;
	float c3= -1.87516686;
	float d3= .896241023;
	float c4= -.930449120;
	float temp,y;
	static float w1=ZERO,w11=ZERO,w12=ZERO,w2=ZERO,w21=ZERO;
	static float w22=ZERO,w3=ZERO,w31=ZERO,w32=ZERO,w4=ZERO;
	static float w41=ZERO,w42=ZERO;


	
	w1=c*x-c1*w11-d1*w12;
	temp=w1+a1*w11+w12;
	w12=w11;
	w11=w1;
	w2=temp-c2*w21-d2*w22;
	temp=w2+a2*w21+w22;
	w22=w21;
	w21=w2;
	w3=temp-c3*w31-d3*w32;
	temp=w3+a3*w31+w32;
	w32=w31;
	w31=w3;
	w4=temp-c4*w41;
	y=w4+w41;
/* w42 set but not used in lowpass */
	w42=w41;
	w41=w4;
	return(y);
}

