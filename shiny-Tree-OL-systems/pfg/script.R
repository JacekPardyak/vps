library("Rcpp")
cppFunction("
NumericVector addOneToVector(NumericVector vector) {
  int n = vector.size();

  for (int i = 0; i < n; ++i)
    vector[i] = vector[i] + 1.0;

  return vector;
}
")

addOneToVector
addOneToVector(1)
