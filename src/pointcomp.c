#include <math.h>
#include "pointcomp.h"

double mandpoint(double cx, double cy, int mi){
	double x = 0.0, y = 0.0, x2, y2;
	int cnt = 0;
	while(cnt++ < mi){
		x2 = x*x;
		y2 = y*y;
		if((x2 + y2) > 4.0)
			return (double)cnt; //(1.0 - log(0.5 * log(x2+y2)) + cnt);
		x = x2 - y2 + cx;
		y = 2.0*x*y + cy;
	}
	return 0.0;  //We haven't escaped
}
