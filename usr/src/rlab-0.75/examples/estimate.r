// -------------------------------------------------------------------
//
// Beginning of codes to test memory allocation/deallocation.
//
// The following lines are in a file called `estimate.r'
initstate = function(v) 
{
  local (prob, tmp, i);

  tmp = 0;
  prob = rand();
  for(i in 1:v.nr) {
    tmp = tmp + v[i;1];
    if (tmp >= prob) {
      break;
    }
  }
  if (v[i;1] == 0) {
    error("initstate: Attempting to initialize to invalid state.");
  }
  return i;
}

nextstate = function(thisstate, trans) 
{
  local (prob, tmp, i);

  tmp = 0;
  prob = rand();
  for(i in 1:trans.nc) {
    tmp = tmp + trans[thisstate;i];
    if (tmp >= prob) {
      break;
    }
  }
  if (trans[thisstate;i] == 0) {
    error("nextstate: Attempting transition to invalid state.");
  }
  return i;
}

randwalk = function(A, P, f, init, h, p, chlen) 
{
//
// Randwalk uses the transition matrix P, the initial state init,
// and the number of steps chlen to take a random walk.  The other
// input variables are required in order to keep the statistics we
// need to accumulate. 
//
  local (old, new, i, W, total);

  old = init;
  W = 1.0;
  total = f[old;1];
  for (i in 1:chlen) {
    new=nextstate(old, P);
    W = W * A[old;new] ./ P[old;new];
    total = total + W * f[new;1];
    old = new;
  }
  return h[init;1] * total ./ p[init;1];
}

estimate = function(B, f, h, chlen, niters) 
{
// Given a matrix equation Bx = f with known coefficient matrix B and
// known right-hand-side f, the function `estimate' uses a Markov chain
// technique to approximate the inner product of the known vector h
// with the unknown vector x (or rather, the inner product of h with the
// chlen+1st iterate of the fixed point iteration x = Ax+f, where
// B = I - A).  The input `niters' controls how many random walks are taken.
// For details of the method, see Chapter 5 of R. Rubinstein's ``Simulation
// and the Monte Carlo Method.''
  local (A, n, i, j, tmp, P, p, total, eta);
  n = B.nr;
  A = eye(n,n) - B;
//
// Now generate P, the ergodic Markov chain, by scaling the rows of A so
// that the row-sums are all = 1.  Note: we are making an implicit assumption
// that the infinity norm of the matrix A is less than one.  (see Rubinstein).
//
  P=zeros(n,n);
  for(i in 1:n) {
    tmp = mysum(abs(A[i;]));
    P[i;] = abs(A[i;])./tmp;
  }
//
// Generate the initial distribution vector p.
//
  p=abs(h)/mysum(abs(h));
//
// Now we're ready to take some `walks'.
//
  total = 0;
  for(i in 1:niters) {
//
// Generate an initial state for the particle, and then call randwalk.
// Randwalk will keep track of the statistics we need for each walk.
//
    j = initstate(p);
//  printf("Walk no. %d...",i);
    eta = randwalk(A, P, f, j, h, p, chlen);
//  printf("done\n");
    total = total + eta;
  }
  return total/niters;
}

//
// End of file `estimate.r'
//

mysum = function(X) 
{
  local(tmp, i, j);

  //
  // Treat vectors, scalars and matrices differently.  If X is a scalar,
  // do nothing...

  if (X.class == "scalar") {
    return X;
  }

  //
  // if X is a matrix, return the vector of row-sums, except in the special
  // case that X.nc==1 or X.rc==1; in that case, return the scalar sum.
  //

  if (X.class == "matrix") 
  {
    if (X.nc == 1) {
      tmp = 0;
      for(i in 1:X.nr) {
        tmp = tmp + X[i;1];
      }
      return tmp;
    }
    if (X.nr == 1) {
      tmp = 0;
      for(i in 1:X.nc) {
        tmp = tmp + X[1;i];
      }
      return tmp;
    }
    tmp = zeros(X.nr);
    for(i in 1:X.nr) {
      for(j in 1:X.nc) {
        tmp[i] = tmp[i] + X[i;j];
      }
    }
  return tmp;
  }
}
