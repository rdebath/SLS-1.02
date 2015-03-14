//
//SKEW  SKEW(A) is the skew-symmetric (Hermitian) part of A, (A-A')/2.
//      It is the nearest skew-symmetric (Hermitian) matrix to A in both the
//      2- and the Frobenius norms.
//

skew = function( A )
{
  return (A - A')./2;
}
