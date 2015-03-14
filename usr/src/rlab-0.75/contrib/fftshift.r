// fftshift -- swap two halves of a vector or swap quadrants of a matrix

fftshift = function(M) {
  local(F,Top,Bot,Left,Right,N);

  if (class(M) != "matrix") {
    error("fftshift: A matrix argument is required");
  }

  // Assign the matrix to the result since if the height or width is odd,
  // one row or column will never be assigned.
  F = M;

  // If we have a vector, then it's easy.
  if (isvector(F)) {
    N = length(F);
    // Swap the right and left halves of the matrix.
    Left  = F[1:floor(N/2)];
    Right = F[ceil(N/2)+1:N];

    // Do the swap.
    F[1:floor(N/2)] = Right;
    F[ceil(N/2)+1:N] = Left;

    return F;
  }
    
  // Swap the top and bottom half of each column. First put the top
  // rows and bottom rows into separate matrices.
  Top = F[1:floor(F.nr/2);];
  Bot = F[ceil(F.nr/2)+1:F.nr;];

  // Now replace the top with the bottom and the bottom with the top.
  F[1:floor(F.nr/2);] = Bot;
  F[ceil(F.nr/2)+1:F.nr;] = Top;

  // Swap the right and left halves of the matrix.
  Left  = F[;1:floor(F.nc/2)];
  Right = F[;ceil(F.nc/2)+1:F.nc];

  // Do the swap.
  F[;1:floor(F.nc/2)] = Right;
  F[;ceil(F.nc/2)+1:F.nc] = Left;

  return F;
}
