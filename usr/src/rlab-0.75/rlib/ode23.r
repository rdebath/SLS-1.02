//-------------------------------------------------------------------//
//	ode23.r
// 	Integrate a system of ordinary differential equations using
// 	2nd and 3rd order Runge-Kutta formulas.
// 	ode23(ufun; t0; tf; y0) integrates the system
// 	of ordinary differential equations described by the 
//	user-function ufun() over the interval t0 to tf and using 
//	initial	conditions y0.
// 	ode23(ufun; t0; tf; y0; tol; 1) uses tolerance tol
// 	and displays status while the integration proceeds.
// 

ode23 = function(Ufun, t0, tfinal, y0, tol, trace) 
{
  local(delta, h, hmax, hmin, pow, s1, s2, s3, t, tau, tout, y, yout);

  //  Initialization
  pow = 1/3;

  if(tol == 0) { tol = 0.001; }

  //  Initialization
  t = t0;
  hmax = (tfinal - t)/5;
  hmin = (tfinal - t)/20000;
  h = (tfinal - t)/100;
  y = y0[:];
  tout = t;
  yout = y.';
  tau = tol * max([norm(y, "i"), 1]);

  if(trace) {
    [t, h, y[:]']
  }

  //  The main loop
  while((t < tfinal) && (h >= hmin)) {
    if(t + h > tfinal) { h = tfinal - t; }

    // Compute the slopes
    s1 = Ufun(t, y);
    s2 = Ufun(t+h, y+h*s1);
    s3 = Ufun(t+h/2, y+h*(s1+s2)/4);

    //Estimate the error and the acceptable error
    delta = norm( h*(s1 - 2*s3 + s2)/3, "i" );
    tau = tol*max( [norm(y,"i"), 1.0] );

    // Update the solution only if the error is acceptable
    if(delta <= tau) 
    {
      t = t + h;
      y = y + h*(s1 + 4*s3 + s2)/6;
      tout = [tout; t];
      yout = [yout; y.'];
    }

    if(trace) { [t, h, y[:]']? }

    //Update the step size
    if(delta != 0.0) {
      h = min( [ hmax, 0.9*h*(tau/delta)^pow ] );
    }
  }

  if(t < tfinal) {
    printf("SINGULARITY LIKELY, t = %f, h = %f\n", t, h);
    error("Error in ode23()");
  }

  return [tout, yout];
};
