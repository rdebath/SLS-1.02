//
// From MATRIX Computations, G.H. Golub, C.F. Van Loan
//

//
// Create a LIST to hold the Householder related functions.
// These functions have only been used with REAL inputs.
//

house = <<>>;

//
// house.v(): Given an N-vector X, generate an n-vector V
// with V[1] = 1, such that (eye(n,n) - 2*(V*V')/(V'*V))*X
// is zero in all but the 1st component.
//

  house.v = function(x)
  {
    local(b, n, u, v);
  
    v = x;
    n = max( size(x) );
    u = norm(x, "2");
    if(u != 0)
      {
	b = x[1] + sign(x[1])*u;
	if(n > 1) {
	  v[2:n] = v[2:n]/b;
	}
      }
    v[1] = 1;
    return v;
  };

//
// house.row(): Given an MxN matrix A and a non-zero M-vector V
// with V[1] = 1, the following algorithm overwrites A with 
// P*A, where P = eye(m,m) - 2*(V*V')/(V'*V)
//

  house.row = function(A, v)
  {
    local(a, b, w);
    
    a = A;
    b = -2/(v'*v);
    w = b*A'*v;
    a = A + v*w';
    return a;
  };

//
// house.col(): Given an MxN matrix A, and an N-vector V, 
// with V[1] = 1, the following algorithm overwrites A with A*P
// where P = eye(N,N) - 2*(V*V')/(V'*V)
//
 
  house.col = function(A,v)
  {
    local(a, v, b, w);

    b = -2/(v'*v);
    w = b*A*v;
    a = A + w*v';

    return a;
  };

//
// Given A, with M >= N, the follwoing function finds Householder
// matrices H1,...Hn, such that if Q = H1*...Hn, then Q'*A = R is
// upper triangular.
//

house.qr = function(A)
  {
    local(a,j,n,m,v);

    a = A;
    m = size(A)[1]; n = size(A)[2];
    v = zeros(m,1);

    for(j in 1:n)
      {
        v[j:m] = house.v( a[j:m;j] );
        a[j:m;j:n] = house.row( a[j:m;j:n], v[j:m] );
	if(j < m) {
	a[ (j+1):m;j ] = v[(j+1):m];
        }
      }
    return a;
  };

//
//  Generate P matrix
//

P = function(V)
{
  local( m );

  m = max( size(V) );
  return [ eye(m,m) - 2*(V*V')./(V'*V) ];
}
