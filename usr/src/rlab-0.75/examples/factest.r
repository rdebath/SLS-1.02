factest = function ( n )
{
  local( i , x );
  i = 0; x = 50;
  while( i < n )
  {
    fac( x );
    i++;
  }
  printf("factest done, n = %i\n", n);
}

