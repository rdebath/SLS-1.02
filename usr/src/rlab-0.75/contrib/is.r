// Boolean tests
isvector = function(M) {
  if (class(M) != "matrix") { return 0; }
  return ((M.nr*M.nc) == M.n);
}

issamesize = function(X,Y) {
  return ((X.nr == Y.nr) && (X.nc == Y.nc));
}

islist = function(X) {
  return (class(X) == "list");
}

isscalar = function(X) {
  return (class(X) == "scalar");
}

isstring = function(X) {
  return (class(X) == "string");
}

isinf2 = function(X) {
  return (abs(X) == inf());
}

isnan2 = function(X) {
  return (X!=X);
}
