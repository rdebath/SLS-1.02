t->screen=(int *)malloc(t->li*t->co*sizeof(int));
t->ary=(struct hentry *)malloc(sizeof(struct hentry)*(t->co+1));
msetI(t->screen,' ',t->li*t->co);
for(x=0;x!=NQUICK;++x) t->quick[x].s=(char *)malloc(t->co+1);
t->quickies=0;
t->tattr=0;
t->placex=0;
t->placey=0;
t->cnt=0;
t->upd=1;
t->updy=0;

t->A=(unsigned long *)malloc(t->li*sizeof(unsigned long));
t->B=(unsigned long *)malloc(t->li*sizeof(unsigned long));
t->mat=(struct mat *)malloc((t->li+2)*(t->li+2)*sizeof(struct mat));

/* Determine the best places to use cd and ce */

static int clreol(t,cs,s)
SCRN *t;
register int *cs,*s;
{
int best=0,bestx= -1;
register gain= -t->cce, ldist=3, gdist=3, x=t->co;
s+=t->co; cs+=t->co;
if(t->ce)
 do
  {
  --x; --s; --cs;
  if(ldist!=3) ++ldist;
  if(gdist!=3) ++gdist;
  if(*cs!=' ')
   if(*s==' ') gain+=gdist, gdist=0;
   else if(*s== *cs) gain-=ldist, ldist=0;
   else gdist=0, ldist=0;
  else if(*s!=' ') gdist=0, ldist=0;
  if(gain>best) best=gain, bestx=x;
  }
  while(x);
return bestx;
}

static int clreos(t)
SCRN *t;
{
int x,y=t->li,z=t->li*t->co,cst=0;
do
 {
 int lcst=0;
 --y;
 x=t->co;
 do
  {
  --x, --z;
  if(t->scrn[z]!=' ') ++lcst;
  if(t->screen[z]!=' ')
   {
   ++x; ++z;
   if(x==t->co) x=0, ++y;
   goto done;
   }
  } while(x);
 cst+=lcst;
 }
 while(y);
done:
if(cst>t->cce) return y;
else return -1;
}

static void magic(t,y,cs,s)
SCRN *t;
int y,*cs,*s;
{
struct hentry htab[256];
int aryx=1;
int x;
if(!(t->im || t->ic || t->IC) ||
   !(t->dc || t->DC)) return;
mset(htab,0,256*sizeof(struct hentry));
msetI(t->ofst,0,t->co);

/* Build hash table */
for(x=0;x!=t->co-1;++x)
 t->ary[aryx].next=htab[cs[x]&255].next,
 t->ary[aryx].loc=x,
 ++htab[cs[x]&255].loc,
 htab[cs[x]&255].next=aryx++;

/* Build offset table */
for(x=0;x<t->co-1;)
 if(htab[s[x]&255].loc>=15) t->ofst[x++]= t->co-1;
 else
  {
  int aryy;
  int maxaryy;
  int maxlen=0;
  int best=0;
  int bestback=0;
  int z;
  for(aryy=htab[s[x]&255].next;aryy;aryy=t->ary[aryy].next)
   {
   int amnt,back;
   int tsfo=t->ary[aryy].loc-x;
   int cst= -Iabs(tsfo);
   int pre=32;
   for(amnt=0;x+amnt<t->co-1 && x+tsfo+amnt<t->co-1;++amnt)
    {
    if(cs[x+tsfo+amnt]!=s[x+amnt]) break;
    else if(s[x+amnt]&255!=32 || pre!=32) ++cst;
    pre=s[x+amnt]&255;
    }
   pre=32;
   for(back=0;back+x>0 && back+tsfo+x>0;--back)
    {
    if(cs[x+tsfo+back-1]!=s[x+back-1]) break;
    else if(s[x+back-1]&255!=32 || pre!=32) ++cst;
    pre=s[x+back-1]&255;
    }
   if(cst>best) maxaryy=aryy, maxlen=amnt, best=cst, bestback=back;
   }
  if(!maxlen) t->ofst[x]=t->co-1, maxlen=1;
  else if(best<2) for(z=0;z!=maxlen;++z) t->ofst[x+z]=t->co-1;
  else for(z=0;z!=maxlen-bestback;++z)
   t->ofst[x+z+bestback]=t->ary[maxaryy].loc-x;
  x+=maxlen;
  }

/* Apply scrolling commands */

for(x=0;x!=t->co-1;++x)
 {
 int q=t->ofst[x];
 if(q && q!=t->co-1)
  if(q>0)
   {
   int z,fu;
   for(z=x;z!=t->co-1 && t->ofst[z]==q;++z);
   while(s[x]==cs[x] && x<t->placex) ++x;
   dodelchr(t,x,y,q);
   for(fu=x;fu!=t->co-1;++fu) if(t->ofst[fu]!=t->co-1) t->ofst[fu]-=q;
   x=z-1;
   }
  else
   {
   int z,fu;
   for(z=x;z!=t->co-1 && t->ofst[z]==q;++z);
   while(s[x+q]==cs[x+q] && x-q<t->placex) ++x;
   doinschr(t,x+q,y,s+x+q,-q);
   for(fu=x;fu!=t->co-1;++fu) if(t->ofst[fu]!=t->co-1) t->ofst[fu]-=q;
   x=z-1;
   }
 }
}

static void udline(t,y,cs,s)
register SCRN *t;
int y;
register int *cs,*s;
{
register int x,bestx,destx=t->co-1,z;
magic(t,y,cs,s);
bestx=clreol(t,cs,s);
for(x=0;x!=destx;++cs,++s,++x)
 {
 if(have) return;
 if(x==bestx)
  {
  cpos(t,x,y);
  attr(t,0);
  texec(t->cap,t->ce,1);
  msetI(cs,' ',t->co-x);
  }
 if(*cs!= *s)
  {
  if(t->ins) clrins(t);
  if(t->os && t->eo && (*cs&255!=' ' || *cs&~*s&~255))
   cpos(t,x,y), outatr(t,' ',0);
  else if(t->ul && *s&255=='_')
   cpos(t,x,y), outatr(t,' ',0);
  cpos(t,x,y), outatr(t,*cs= *s,0);
  }
 }
}

void ndoquick(t)
SCRN *t;
{
int n,x;
for(n=0;n!=t->quickies;++n)
 {
 int *s=t->screen+t->quick[n].y*t->co+t->quick[n].x,
  *cs=t->scrn+t->quick[n].y*t->co+t->quick[n].x;
 if(t->quick[n].ch)
  {
  *s=t->quick[n].c|t->quick[n].attr;
  if(*s != *cs)
   {
   if(t->ins) clrins(t);
   if(t->os && t->eo && (*cs&255!=' ' || *cs&~*s&~255))
    cpos(t,t->quick[n].x,t->quick[n].y), outatr(t,' ',0);
   else if(t->ul && *s&255=='_')
    cpos(t,t->quick[n].x,t->quick[n].y), outatr(t,' ',0);
   cpos(t,t->quick[n].x,t->quick[n].y), outatr(t,*cs= *s,0);
   }
  }
 else
  for(x=0;x!=t->quick[n].l;++x,++s,++cs)
   {
   *s=t->quick[n].s[x]|t->quick[n].attr;
   if(*s != *cs)
    {
    if(t->ins) clrins(t);
    if(t->os && t->eo && (*cs&255!=' ' || *cs&~*s&~255))
     cpos(t,t->quick[n].x+x,t->quick[n].y), outatr(t,' ',0);
    else if(t->ul && *s&255=='_')
     cpos(t,t->quick[n].x+x,t->quick[n].y), outatr(t,' ',0);
    cpos(t,t->quick[n].x+x,t->quick[n].y), outatr(t,*cs= *s,0);
    }
   }
 }
t->quickies=0;
}

void ndumpq(t)
SCRN *t;
{
int n,x;
if(t->quickies) t->upd=1;
for(n=0;n!=t->quickies;++n)
 {
 int *s=t->screen+t->quick[n].y*t->co+t->quick[n].x;
 t->updtab[t->quick[n].y]=1;
 if(t->quick[n].ch)
  *s=t->quick[n].c|t->quick[n].attr;
 else
  for(x=0;x!=t->quick[n].l;++x,++s)
   *s=t->quick[n].s[x]|t->quick[n].attr;
 }
t->quickies=0;
}


void nrefresh(t)
SCRN *t;
{
int y;
int *cs, *s;

ttflsh();
/*
if(t->quickies && !t->upd && !t->cnt) ndoquick(t);
if(t->quickies && (t->upd || t->cnt)) ndumpq(t);
if(!t->upd && !t->cnt) goto check;

t->cnt=0;
if(!t->upd && t->updy>=t->placey) goto after;
if(!t->upd) goto before;

nscroll(t);

y=t->placey;
after:
cs=t->scrn+t->co*y;
s=t->screen+t->co*y;
for(;y!=t->li;++y, s+=t->co, cs+=t->co)
 if(have)
  {
  t->updy=y;
  t->cnt=1;
  cpos(t,t->placex,t->placey);
  return;
  }
 else
  if(t->updtab[y])
   {
   udline(t,y,cs,s);
   if(!have) t->updtab[y]=0;
   }

y=0;
before:
cs=t->scrn+t->co*y;
s=t->screen+t->co*y;
for(;y!=t->placey;++y,s+=t->co,cs+=t->co)
 if(have)
  {
  t->updy=y;
  t->cnt=1;
  cpos(t,t->placex,t->placey);
  return;
  }
 else
  if(t->updtab[y])
   {
   udline(t,y,cs,s);
   if(!have) t->updtab[y]=0;
   }
*/
check:
if(t->x!=t->placex || t->y!=t->placey) cpos(t,t->placex,t->placey);
t->upd=0;
}


void nprintf(t,x,y,str,a1,a2,a3,a4,a5,a6,a7)
SCRN *t;
int x,y;
char *str;
{
if(t->quickies==NQUICK) ndumpq(t);
t->quick[t->quickies].ch=0;
t->quick[t->quickies].x=x;
t->quick[t->quickies].y=y;
t->quick[t->quickies].attr=t->tattr;
sprintf(t->quick[t->quickies].s,str,a1,a2,a3,a4,a5,a6,a7);
t->quick[t->quickies].l=strlen(t->quick[t->quickies].s);
++t->quickies;
}

void nputs(t,x,y,s)
SCRN *t;
int x,y;
char *s;
{
if(t->quickies==NQUICK) ndumpq(t);
t->quick[t->quickies].ch=0;
t->quick[t->quickies].x=x;
t->quick[t->quickies].y=y;
t->quick[t->quickies].l=zlen(s);
t->quick[t->quickies].attr=t->tattr;
strcpy(t->quick[t->quickies].s,s);
++t->quickies;
}


void nsetpos(t,x,y)
SCRN *t;
int x,y;
{
t->placex=x;
t->placey=y;
}

void ngetpos(t,x,y)
SCRN *t;
int *x,*y;
{
*x=t->placex;
*y=t->placey;
}

int boxul='+', boxur='+', boxll='+', boxlr='+', boxl='|', boxr='|', boxt='-',
  boxb='-';

void nbox(t,x,y,w,h)
SCRN *t;
int x,y,w,h;
{
int z;
ndumpq(t);
t->screen[x+(y-1)*t->co-1]=(boxul|t->tattr);
t->screen[x+w+(y-1)*t->co]=(boxur|t->tattr);
t->screen[x+(y+h)*t->co-1]=(boxll|t->tattr);
t->screen[x+w+(y+h)*t->co]=(boxlr|t->tattr);
msetI(t->screen+x+(y-1)*t->co,boxt|t->tattr,w);
msetI(t->screen+x+(y+h)*t->co,boxb|t->tattr,w);
for(z=0;z!=h;++z) t->screen[x+(y+z)*t->co-1]=(boxl|t->tattr),
                  t->updtab[y+z]=1;
for(z=0;z!=h;++z) t->screen[x+w+(y+z)*t->co]=(boxr|t->tattr);
t->upd=1;
}

void nrect(t,x,y,w,h,c)
SCRN *t;
int x,y,w,h,c;
{
int i,j;
ndumpq(t);
c|=t->tattr;
for(j=0;j!=h;++j)
 {
 msetI(t->screen+x+(y+j)*t->co,c,w);
 t->updtab[y+j]=1;
 }
t->upd=1;
}
int *nsave(t,x,y,w,h)
SCRN *t;
int x,y,w,h;
{
int *tmp=(int *)malloc(w*h*sizeof(int));
int j;
for(j=0;j!=h;++j) mcpy(tmp+j*w,t->screen+x+(j+y)*t->co,w*sizeof(int));
return tmp;
}

void nrestore(t,x,y,w,h,s)
SCRN *t;
int x,y,w,h,*s;
{
int j;
ndumpq(t);
for(j=0;j!=h;++j)
 {
 mcpy(t->screen+x+(j+y)*t->co,s+j*w,w*sizeof(int));
 t->updtab[j+y]=1;
 }
t->upd=1;
}


void ngosling(t)
SCRN *t;
{
int x,y,ox,oy,*s,*cs;

/* These really need to be determined from termcap costs and should vary
   for each line.  To be fixed in the future... */
int icost=5, 	/* Cost to insert */
  dcost=5, 	/* Cost to delete */
  rcost=35;	/* Cost to replace the line */

if(!t->cs && !t->al && !t->AL) return;

ndumpq(t);

/* Generate IDs for the lines */
/* This is where all the CPU time gets wasted */
s=t->screen;
cs=t->scrn;
for(y=0;y!=t->li;++y)
 {
 t->A[y]=0; t->B[y]=0;
 x=t->co; do
  t->A[y]=t->A[y]*34857+*cs++ -' ',
  t->B[y]=t->B[y]*34857+*s++ - ' ';
  while(--x);
 }

/* Clear matrix */
mset(t->mat,0,sizeof(struct mat)*(t->li+2)*(t->li+2));

/* Solve the problem using dynamic programming technique */

/* Initialize edges of matrix */
for(x=1;x!=t->li+2;x++) t->mat[x].cost=32767;
for(y=1;y!=t->li+2;y++) t->mat[y*(t->li+2)].cost=32767;

/* Fill in rest of matrix */
for(y=1;y!=t->li+2;y++)
 for(x=1;x!=t->li+2;x++)
  {
  int i,d,r;
  r=((t->A[y-2]==t->B[x-2]&&t->B[x-2])?0:rcost)+
    t->mat[(y-1)*(t->li+2)+(x-1)].cost;
  d=dcost+t->mat[(y-1)*(t->li+2)+(x)].cost;
  i=icost+t->mat[(y)*(t->li+2)+(x-1)].cost;
  if(r<=i && r<=d) t->mat[y*(t->li+2)+x].cost=r, t->mat[y*(t->li+2)+x].from=0;
  else if(i<=r && i<=d)
   t->mat[y*(t->li+2)+x].cost=i, t->mat[y*(t->li+2)+x].from=1;
  else
   t->mat[y*(t->li+2)+x].cost=d, t->mat[y*(t->li+2)+x].from=2;
  }

/* Trace result and apply scrolling commands */

x=t->li+1, y=t->li+1;
while(x>1 && y>1)
 switch(t->mat[y*(t->li+2)+x].from)
  {
 case 0:
 	ox=x, oy=y;
        do x--, y--; while(t->mat[y*(t->li+2)+x].from==0 && (x>1 || y>1)); 
        if(x>y) dodnscrl(t,y-1,ox-1,x-y);
        else if(x<y) doupscrl(t,x-1,oy-1,y-x);
        break;
 case 1:
 	x--;
 	break;
 case 2:
	y--;
        break;
  }
}
