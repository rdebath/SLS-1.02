#include "cs.h"			/*						UGENS6.C	*/
#include "ugens6.h"
#include <math.h>

static float	fzero = 0., fone = 1.;

/* the following is based on code from Michael Clarke, Huddersfield Polytechnic */

fofset(p)
 register FOFS	*p;
{
	register FUNC	*ftp1, *ftp2;
	register long	xds;

	if ((ftp1 = ftfind(p->ifna)) != NULL
	 && (ftp2 = ftfind(p->ifnb)) != NULL) {
		p->ftp1 = ftp1;
		p->ftp2 = ftp2;
		p->duration = *p->idur;
		if (*p->iphs != 0.)
			p->fundphs = (long)(*p->iphs * fmaxlen) & PMASK;
		else {
			p->fundphs = 0;
			p->fund_per_flag = TRUE;
		}
		p->clock_incr = onedsr;
		if ((xds = *p->iolaps) <= 0) {
			initerror("illegal value for iolaps");
			return;
		}
		xds *= (LOCAL_BUFFER_SIZE * sizeof(float));
		auxalloc((long)xds, &p->auxch);
		p->fofaux = (float *) p->auxch.auxp;
		p->olaps_int = p->olaps_trig = *p->iolaps;
		p->print_flag = TRUE;
		p->tab_quart = fmaxlen / 4.;
		p->tab_three_quart = p->tab_quart * 3;
		p->tab_quart_flt = p->tab_quart;        /* float version */
		p->ampcod	= (p->XINCODE & 0x2) ? 1 : 0;
		p->fundcod	= (p->XINCODE & 0x1) ? 1 : 0;
		p->formacod	= (p->XINCODE & 0x8) ? 1 : 0;
		p->formbcod	= (p->XINCODE & 0x4) ? 1 : 0;
	}
}

fof(p)
 register FOFS *p;
{
	register FUNC	*ftp1,	*ftp2;
	register float	result,	*ar, *amp, *fund, *forma, *formb;
	register long nsmps = ksmps, overlaps, formphs, texind, attind;
	register float	*localp, formb_incr;
	void	refresh();

	ar = p->ar;
	amp = p->xamp;
	fund = p->xfund;
	forma = p->xforma;
	formb = p->xformb;
	ftp1 = p->ftp1;
	ftp2 = p->ftp2;
	formb_incr = *formb++ * sicvt;
	p->fund_incr = *fund * sicvt;

	do {
		overlaps = *p->iolaps;
		*ar	=	0.0;
		localp = p->fofaux;
		do {                        /* OVERLAPS LOOP */
			if (p->fund_per_flag && p->olaps_trig == overlaps){
				           /* CREATE NEW FOF */
				refresh(p, localp, *amp, *fund, *forma);
				if (p->fund_per_flag) {
					p->fund_per_flag = FALSE;
					goto lookups;
				}
			}
			CLOCK_TIM += p->clock_incr;
			if (CLOCK_TIM < END_TIM) {
  				FORM_PHS += (FORM_INC + formb_incr);
				BAND_CUR *= BAND_EXP;
lookups:			formphs = FORM_PHS;
				formphs = formphs & PMASK;
				FORM_PHS = formphs;
				formphs >>= ftp1->lobits;
				result = *(ftp1->ftable + formphs);
				result *= BAND_CUR;
				if (TEX_IND > p->tab_quart_flt) {
					texind = TEX_IND;
					texind >>= ftp2->lobits;
					result *= (*(ftp2->ftable+texind)+1)/2;
					TEX_IND -= TEX_INC;
				}
				if (ATT_IND > p->tab_quart_flt) {
					attind = ATT_IND;
					attind >>= ftp2->lobits;
					result *= (*(ftp2->ftable+attind)+1)/2;
				}
				ATT_IND += ATT_INC;
				result *= OCT_AMP;
				*ar  += result;
			}
			localp += 11;
		} while (--overlaps);
		p->duration -= p->clock_incr;
		*ar++;

		p->fundphs += p->fund_incr;
		if (p->fundphs < 0L || p->fundphs >= MAXLEN) {
			p->fundphs &= PMASK;
			p->fund_per_flag = TRUE;
		}

		if (p->ampcod)
			(*amp++);
		if (p->fundcod)
			p->fund_incr = *++fund * sicvt;
		if (p->formacod)
			(*forma++);
		if (p->formbcod)
			formb_incr = *formb++ * sicvt;
	} while (--nsmps);
}

void refresh(p,localp,ampl,fundam,form_a)
 register FOFS	*p;
 register float	*localp,ampl,fundam,form_a;
{
	register int	bit_check;
	register float	coef_check, ten_oct, band_negpi, clock_sr;
	register float  beta2, alphbw;   /* for amp corr */

	if (p->duration < (*p->kdebat + *p->katt)) {
		p->fof_count += 1;
		p->fund_per_flag = FALSE;
		return;
	}
	bit_check = 1023;    /* 0x3FF, for 11 coefs */
	coef_check = 0.0;
	if (*p->koct < 1) {
		do {
			if (*p->koct <= coef_check) {
				if (bit_check & p->fof_count) {
					p->fof_count += 1;
					p->fund_per_flag = FALSE;
					return;
				}
				bit_check = 0;
			}
			else {
				bit_check >>= 1;
				coef_check += .1;
			}
		} while (bit_check);
	}
	if (--(p->olaps_trig) == 0)
		p->olaps_trig = p->olaps_int;
	if (p->print_flag) {
		CLOCK_TIM += p-> clock_incr;
		if (CLOCK_TIM < END_TIM) {
			printf("Warning: atten +debat > fund period * iolaps");
			printf("\nMore overlaps may be required\n");
			p->print_flag = FALSE;
		}
	}
	bit_check = 1;
	OCT_AMP = ampl;
	if (*p->koct < 1) {
		coef_check = 10;
		ten_oct = *p->koct * 10;
		do {
			if (p->fof_count & bit_check) {
				bit_check = 1000;
				if (coef_check > ten_oct) {
					coef_check -= fone;
					if (coef_check >= ten_oct) {
						OCT_AMP = fzero;
						END_TIM = fzero;
					}
					else OCT_AMP *= ten_oct - coef_check;
				}
			}
			else {
				coef_check -= fone;
				bit_check << 1;
			}
		} while (bit_check <= 512);
	}
	CLOCK_TIM = p->fundphs / (fundam * fmaxlen);
	FORM_INC = form_a * sicvt;
	clock_sr = CLOCK_TIM * esr;
	FORM_PHS = FORM_INC * clock_sr;
	if (*p->icor != 0.0) {                   /*  amp correction (new) */
	  if (*p->ktex == p->prvtex && *p->kband == p->prvband)
	    OCT_AMP *= p->ampcor;
	  else {
	    p->prvtex = *p->ktex;
	    p->prvband = *p->kband;
	    beta2 = 1.0 / (p->prvtex * p->prvtex);
	    alphbw = p->prvband * p->prvtex * pi;
	    p->ampcor = (pid100/((1./(1.+alphbw*(1.+alphbw*.5))+1.)*beta2)
	                * p->prvband * (p->prvband * p->prvband + beta2));
	    OCT_AMP *= p->ampcor;
	  }
	}
	if (*p->ktex == 0) {
		TEX_INC = 0;
		TEX_IND = p->tab_quart;
	}
	else {
		TEX_INC = sicvt / (*p->ktex * 2);
		TEX_IND = p->tab_three_quart - (TEX_INC * clock_sr);
	}
	band_negpi = *p->kband * NEGPI;
	BAND_CUR = exp(band_negpi * CLOCK_TIM);
	BAND_EXP = exp(band_negpi/esr);
	if (*p->katt < onedsr)
		ATT_INC = 1.;                       /*arbitrary*/
	else    ATT_INC = sicvt / (*p->katt * 2);
	ATT_IND = ATT_INC * esr * (CLOCK_TIM - *p->kdebat) + p->tab_quart;
	END_TIM = *p->kdebat + *p->katt;
	p->fof_count += 1;
}
