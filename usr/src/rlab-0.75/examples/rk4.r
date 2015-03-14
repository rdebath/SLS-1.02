//
//  RLaB 4th Order Runge-Kutta Integrator
//  Fixed step size.
//  Ian Searle, 6/22/92

//
//  Inputs to rk4()
//  y:		State vector
//  dy:		State vector derivative
//  x:		Start time (t)
//  h:  	Time step (dt)
//  eval:	User-Function to evaluate state-derivative.
//		has arguments (time; y; dy)
//  Returns yout: The integration results
//

rk4 = function(y, dydx, x, h, eval) 
{
  local(i, xh, hh, h6, dym, dyt, yt, yout);

  hh = h*0.5;
  h6 = h/6;
  xh = x + hh;

  eval(x, y, dydx);      // 1st step
  yt = y + hh*dydx;

  eval(xh, yt, dyt);     // 2nd step
  yt = y + hh*dyt;

  eval(xh, yt, dym);     // 3rd step
  yt = y + h*dym;
  dym = dym + dyt;

  eval(x+h, yt, dyt);    // 4th step
  yout = y + h6*(dydx + dyt + 2.0*dym);

  return yout;
}

//
//  Driver for above integrator
//  This driver spares the caller the chore
//  of writing a for-loop
//

drk4 = function(y0, dy0, tzero, tfinal, dt, eval) 
{
  local(i, y, dy, yout, out);

  y = y0;
  dy = dy0;

  out = [tzero, y0];
  for(i in tzero:tfinal:dt) 
  {
    out = [out; i+dt, (y = rk4(y, dy, i, dt, eval))];
  }
  return out;
};

