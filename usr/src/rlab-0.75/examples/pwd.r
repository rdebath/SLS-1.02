//
// Randomly generate passwordds
// Ian Searle (11/6/91)
//

alpha = ["a", "b", "c", "d", "e", "f", "g", "h", "i", ...
         "j", "k", "l", "m", "n", "o", "p", "q", "r", ...
         "s", "t", "u", "v", "w", "x", "y", "z" ];

//
// Global set-up
//

srand( "clock" );

pwd = function()
{
  local(i, PWD, tmp);

  tmp = "";
  rand("uniform", 1, 26);
  for(i in 1:6) {
    PWD[i]   = alpha[ irand() ];
  }

  rand("uniform", 0, 9);
  
  sprintf(tmp, "%i", irand());
  PWD[7] = tmp;
  sprintf(tmp, "%i", irand());
  PWD[8] = tmp;

  return PWD;
}

//
// Return an integer random number between 1 and 26
//

irand = function()
{
  return int( rand() );
}
