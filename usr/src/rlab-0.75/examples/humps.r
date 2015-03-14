//
//  Write a function that evaluates a mathematical function.
//

humps = function(x) 
{
  return (1 ./ ((x-.3).^2 + .01) + 1 ./ ((x-.9).^2 + .04) - 6);
}

//
//  The following statements: 
//  Create a vector of the independent variable.

//  x = -1:2:.01;

//
//  Evaluate the function over the specified interval,
//  and write out the result in one statement.

//  write("humps-data"; [x; humps(x)]')

//
//  `hump(x)' evaluates the the function for x.
//  `[x; hump(x)]'' forms a matrix with the independent variable (x)
//  as the 1st column, and the evaluated function as the 2nd column.
//
