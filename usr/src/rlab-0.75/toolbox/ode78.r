// ODE78  Integrates a system of ordinary differential equations using
// 7th order formulas.
//
// out = ode78(F, t0, tfinal, y0, tol, trace)
//
// INPUT:
// Ufun  - String containing name of user-supplied problem description.
//         Call: yprime = fun(t,y) where F = 'fun'.
//         t      - Time (scalar).
//         y      - Solution column-vector.
//         yprime - Returned derivative column-vector; yprime(i) = dy(i)/dt.
// t0    - Initial value of t.
// tfinal- Final value of t.
// y0    - Initial value column-vector.
// tol   - The desired accuracy. (Default: tol = 1.e-6).
// trace - If nonzero, each step is printed. (Default: trace = 0).
//
// OUTPUT:
// out  - Returned integration time points (1st column).
//        Remaining columns are solutions of equations specified in Ufun
//
// The result can be displayed by: plot(tout, yout).

//   Daljeet Singh
//   Dept. Of Electrical Engg., The University of Alabama.
//   11-24-1988.

//   Modified by Ian Searle
//   4-18-93

ode78 = function (Ufun, t0, tfinal, y0, tol, trace)
{
  local (alpha, beta, chi, delta, f, gamma1, h, hmax, ...
         hmin, j, psi, pow, t, tau, tout, yout);

  // The Fehlberg coefficients:
  alpha = [ 2./27., 1/9,  1/6,    5/12,      .5, 5/6, 1/6, 2/3, 1/3, 1, 0, 1 ]';
  beta =  [ 2/27,    0,     0,       0,       0, 0, 0, 0, 0, 0, 0, 0, 0 ;
      1/36, 1/12,     0,       0,       0, 0, 0, 0, 0, 0, 0, 0, 0 ;
      1/24,    0,   1/8,       0,       0, 0, 0, 0, 0, 0, 0, 0, 0 ;
      5/12,    0,-25/16,   25/16,       0, 0, 0, 0, 0, 0, 0, 0, 0 ;
       .05,    0,     0,     .25,      .2, 0, 0, 0, 0, 0, 0, 0, 0 ;
   -25/108,    0,     0, 125/108,  -65/27, 125/54,  0, 0, 0, 0, 0, 0, 0 ;
    31/300,    0,     0,       0,  61/225,    -2/9, 13/900, 0, 0, 0, 0, 0, 0 ;
         2,    0,     0,   -53/6,  704/45,  -107/9,  67/90, 3, 0, 0, 0, 0, 0 ;
   -91/108,    0,     0,  23/108,-976/135,  311/54, -19/60, 17/6, -1/12, 0, 0, 0, 0 ;
 2383/4100, 0, 0, -341/164, 4496/1025, -301/82, 2133/4100, 45/82, 45/164, 18/41, 0,...
                                                                       0, 0 ;
  3/205, 0, 0,  0, 0, -6/41, -3/205, -3/41, 3/41, 6/41, 0, 0, 0 ;
  -1777/4100, 0, 0, -341/164, 4496/1025, -289/82, 2193/4100, ...
  51/82, 33/164, 12/41, 0, 1, 0 ]';

  chi = [0, 0, 0, 0, 0, 34/105, 9/35, 9/35, 9/280, 9/280, 0, 41/840, 41/840]';
  psi = [1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1, -1,  -1 ]';
  pow = 1/8;
  if (name (tol) == "dummy") { tol = 1.e-6; }

  // Initialization
  t = t0;
  // the following step parameters are used in ODE45
  // hmax = (tfinal - t)/5;
  // hmin = (tfinal - t)/20000;
  // h = (tfinal - t)/100;
  // The following parameters were taken because the integrator has
  // higher order than ODE45. This choice is somewhat subjective.
  hmax = (tfinal - t)/2.5;
  hmin = (tfinal - t)/10000;
  h = (tfinal - t)/50;
  y = y0[:];
  f = y*zeros (1,13);
  tout = t;
  yout = y.';
  tau = tol * max( [norm(y, "I"), 1]);

  if (trace) { [clc, t, y[:]'] ? }

  // The main loop

  while ((t < tfinal) && (h >= hmin))
  {
    if (t + h > tfinal) { h = tfinal - t; }

    // Compute the slopes
    f[;1] = Ufun (t, y);
    for (j in 1:12)
    {
      f[;j+1] = Ufun (t+alpha[j]*h, y+h*f*beta[;j]);
    }

    // Truncation error term
    gamma1 = h*41/840*f*psi;

    // Estimate the error and the acceptable error
    delta = norm (gamma1,"I");
    tau = tol*max ([norm (y,"I"),1.0]);

    // Update the solution only if the error is acceptable
    if (delta <= tau)
    {
      t = t + h;
      y = y + h*f*chi;
      tout = [tout; t];
      yout = [yout; y.'];
    }
    if (trace) { [home, t, y[:]']; }

    // Update the step size
    if (delta != 0.0)
    {
      h = min ([hmax, 0.8*h*(tau/delta)^pow]);
    }
  }

  if (t < tfinal)
  {
    fprintf ("stderr", "SINGULARITY LIKELY, at t = %d\n", t);
  }

  return [tout, yout];
}