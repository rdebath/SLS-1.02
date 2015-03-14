#ifndef SYSINFO_H
#define SYSINFO_H

void loadavg(double *av1, double *av5, double *av15);
void meminfo(unsigned *total, unsigned *used, unsigned *free,
	     unsigned *shared, unsigned *buffers);

#endif /* SYSINFO_H */
