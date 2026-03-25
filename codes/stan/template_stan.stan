data {
  int<lower=0> N;  // dimension 1
  int<lower=0> A;  // dimension 2
  int<lower=0> Y_obs;  // dimension 3
  int<lower=0> G;  // dimension 4
  int<lower=0> K;  // dimension 5
  int<lower=0> y[N]; // observed data counts
  vector<lower=0>[N] X;  // example of vector
  matrix[A, K] B; // example of matrix
}

parameters {
  
  matrix<lower=0, upper=45>[Y_obs, G] x0; // example of matrix
  real<lower=0> sigma_x0; // example of variance
  
}

transformed parameters {
  
  matrix<lower=0, upper=1>[A, Y_obs] p[G]; // example of array
  
}

model {
  
  // Convert array of matrices into vector for efficiency
  {
    matrix[A*Y_obs, G] p_ord;
    
    for (g in 1:G) {
      p_ord[, g] = to_vector(p[g]);
    }
    y ~ binomial(n, to_vector(p_ord)); // to_vector(): col major order
  }
  
}
