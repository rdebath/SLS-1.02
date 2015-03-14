//
//  Calculate Fibonacci numbers
//

"Starting the fibonacci test..."
i=1; 
while ( i < 2 ) { 
  i=i+1;
  a=0; b=1;
  while ( b < 10000 ) {
    c = b;
    b = a+b;
    a = c;
  }
}

b

if ( b != 10946 ) {
  error("failed fibonacci test");
else
"...passed fibonacci test..."
}
