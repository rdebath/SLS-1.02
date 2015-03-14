//
//  Integrate the Van def Pol equation
//

vdpol = function ( t, x ) {
  local(xdot);

  xdot[1;1] = x[1] * (1 - x[2]^2) - x[2];
  xdot[2;1] = x[1];
  return xdot;
}

// system( "/usr/ucb/ps -aux | grep rlab" );
t0 = 0;
tf = 20;
x0 = [0; 0.25];
rfile ode78
tic();
out = ode78( vdpol, t0, tf, x0);
printf(" Elapsed integrtion time: %i\n", toc() );
plot( out );
