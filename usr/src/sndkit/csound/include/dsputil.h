/****************************************************************/
/*  dsputil.h							*/
/* DSP utility functions for Csound - dispfft and pvoc		*/
/* Header file - just declarations				*/
/* 20apr90 dpwe							*/
/****************************************************************/

#define     SPTS    (16)    /* SINC TABLE: How many points in each lobe */
#define     SPDS    (6)     /*	 (was 8)   How many sinc lobes to go out */
#define     SBW     0.9     /* To compensate for short sinc, reduce bandw */

#ifndef PI
#ifdef M_PI
#define PI M_PI
#else
#define PI 3.1415926535
#endif
#endif

#ifndef NULL
#define NULL 0L
#endif

/*** yukki ansi bodge ***/
#ifndef FLOATARG
#ifdef __STDC__
#define FLOATARG double		/* you can't have floats in prototypes! */
#else
#define FLOATARG double
#endif /* def __STDC__ */
#endif /* ndef FLOATARG */

/* Predeclare static supporting functions */

#ifdef __STDC__

void CopySamps(float *, float *, int);
float *MakeBuf(int);
float *MakeHalfWin(int, FLOATARG, int);
void FillHalfWin(float *, int, FLOATARG, int);
void UnpackReals(float *, int);
void PackReals(float *, int);
void Rect2Polar(float *, int);
void Polar2Rect(float *, int);
void Lin2DB(float *, int);
void DB2Lin(float *, int);
float maskPhs(FLOATARG);
void UnwrapPhase(float *, int, float *);
void RewrapPhase(float *, int, float *);
void PhaseToFrq(float *, int, FLOATARG, FLOATARG);
void FrqToPhase(float *, int, FLOATARG, FLOATARG, FLOATARG);
void FetchIn(float *, float *, int, FLOATARG);
void FillFFTnConj(float *, int, FLOATARG, int);
void ApplyHalfWin(float *, float *, int);
void addToCircBuf(float *, float *, int, int, int);
void writeClrFromCircBuf(float *, float *, int, int, int);
void FixAndAdd(float *, short *, int);
long NumFrames(long, int, int);
long NumSampls(long, int, int);
void FloatAndCopy(short *, float *, int);
void WriteOut(float *, float **, int);
void UDSample(float *, FLOATARG, float *, int, int, FLOATARG);
void MakeSinc();
void DestroySinc();	  /* relase the lookup table */
float SincIntp(FLOATARG); /* return interpolated sinc - not used */
void PreWarpSpec(float *, int, FLOATARG);

#else

void CopySamps();
float *MakeBuf();
float *MakeHalfWin();
void FillHalfWin();
void UnpackReals();
void PackReals();
void Rect2Polar();
void Polar2Rect();
float maskPhs();
void UnwrapPhase();
void RewrapPhase();
void PhaseToFrq();
void FrqToPhase();
void FetchIn();
void FillFFTnConj();
void ApplyHalfWin();
void addToCircBuf();
void writeClrFromCircBuf();
void FixAndAdd();
long NumFrames();
long NumSampls();
void FloatAndCopy();
void WriteOut();
void UDSample();
void MakeSinc();
void DestroySinc();	  /* relase the lookup table */
float SincIntp(); /* return interpolated sinc - not used */
void PreWarpSpec();

#endif
