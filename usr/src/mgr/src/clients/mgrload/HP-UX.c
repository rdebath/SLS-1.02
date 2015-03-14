#include <sys/pstat.h>
    
struct pst_dynamic dynamic;

double getload(void)
{
  pstat(PSTAT_DYNAMIC,(union pstun)&dynamic,sizeof(dynamic),0,0);
  return (dynamic.psd_avg_1_min);
}
