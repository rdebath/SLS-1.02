//
//JORDAN  JORDAN(N, LAMBDA) is the N-by-N Jordan block with eigenvalue LAMBDA.
//        LAMBDA = 1 is the default.

jordan = function(n, lambda)
{
  if( name(lambda) == "dummy" ) { lambda = 1; }
  return ( lambda*eye(n,n) + diag(ones(n-1,1),1) );
}
