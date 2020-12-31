#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// This function allows to do convolutions on the edge elements of the 
// matrix since it extracts elements from the opposite rows or columns 
// to avoid losing dimensionality after convolution
int get_index(int M, int i)
{
  if (i < 0)
    return (M + i % M) % M;
  if(i >= M)
    return i % M;
  return i;
}


// This function performs a single iteration of the Cyclic CA
// [[Rcpp::export]]
arma::mat iterate_cyclic(arma::mat X, 
                         Rcpp::DataFrame L, 
                         int s,
                         int t){
  int m = X.n_rows;
  int n = X.n_cols;
  int k = L.nrows();
  
  Rcpp::IntegerVector dx = L["x"];
  Rcpp::IntegerVector dy = L["y"];

  arma::mat X_new(m, n);
  
  for(int x = 0; x < m; x++) {
    for(int y = 0; y < n; y++){
      int c = 0;
      int v = X(x,y);
      int v_n = (v+1) % s;
      for(int z = 0; z < k; z++){
        int ix  = get_index(m, x + dx[z]);
        int iy  = get_index(n, y + dy[z]);
        int X_n = X(ix, iy);
        if (X_n == v_n)
        {
         ++c; 
        }
      }
      if (c >= t)
      {
        X_new(x,y) = v_n;
      }
      else
      {
        X_new(x,y) = v;
      }
    }
  }
  return X_new;
};




