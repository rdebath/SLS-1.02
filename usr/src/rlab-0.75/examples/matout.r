//-------------------------------------------------------------------//
//
//  Syntax:	matout ( m , file )

//  Description:

//  Write out a matrix in a MATLAB compatible format. This really just
//  outputs a matrix in RLaB format 

//-------------------------------------------------------------------//

matout = function( m , file )
{
  local(i,j);

  fprintf(file, " %s = [", name(m));
  for(i in 1:m.nr) {
    for(j in 1:m.nc) {
      if(j != m.nc) {
        fprintf(file, " %f,", m[i;j]);
      else
        fprintf(file, " %f", m[i;j]);
      }
    }
    if(i != m.nr) { fprintf(file, ";\n\t"); }
  }
  fprintf(file, "]\n");
}

