data {
  int<lower=0> N;  // nber of cells
  int<lower=0> A_all;  // nber of ages: 15-50
  int<lower=0> T_all; // nber of years
  array[A_all] real<lower=0> age;  // ages: 15-50
  int<lower=0> G; // nber of groups (race * edu). Order is edu, race
  array[N] int<lower=0> obs_i; // indexing variable for age in data (needed if missing value for the likelihood)
  vector[N] logit_p; // # data is logit estimate of p from survey
  vector<lower=0>[N] logit_se; // se associated with  on logit scale (-1 as no error at age 14 yo)
}

parameters {
  
  matrix<lower=0, upper=1>[T_all, G] omega;
  matrix<lower=0, upper=1>[T_all, G] gamma;
  matrix<lower=0>[T_all, G] delta;
  // Relaxed constraint on delta (sigmoidal not really present for LHS and HS)
  //matrix<lower=1>[T_all, G] delta;
  
  // Changed to allow changes over time
  matrix<lower=0, upper=1>[T_all, G] alpha;

  // Complete pooling
  //-----------------
  real<lower=0> sigma_o;
  real<lower=0> sigma_g;
  real<lower=0> sigma_d;
  
  real<lower=0> sigma_a;
}

transformed parameters {
  
  array[G] matrix<lower=0, upper=1>[A_all, T_all] jano;
  matrix<lower=0, upper=1> [A_all * T_all, G] jano_ord;

  array[G] matrix<lower=0, upper=1>[A_all, T_all] phi;
  
  for (g in 1:G) {
    for (t in 1:T_all) {
    for (x in 1:A_all) {
        // Janoscheck sigmoidal function
      jano[g][x,t]  = omega[t,g] - (omega[t,g] - alpha[t,g]) * exp(-gamma[t,g] * pow(age[x], delta[t,g]));
    }
  }
  phi[g] = 1 - jano[g];
  
  // Reorder mu to be able to get a vector
  jano_ord[, g] = to_vector(jano[g]); // to_vector(): col major order
  }
  
}

model {
  
  // Likelihood 
  target += normal_lpdf(logit_p | logit(to_vector(jano_ord)[obs_i]), logit_se);
  
  to_vector(alpha[1:2, ]) ~ normal(0.3, 0.2);
  to_vector(omega[1:2, ]) ~ normal(0.8, 0.25);
  to_vector(gamma[1:2, ]) ~ normal(0.5, 1);
  to_vector(delta[1:2, ]) ~ normal(1, 1);
  // Relaxed constraint on delta (sigmoidal not really present for LHS and HS)
  // to_vector(delta[1:2, ]) ~ normal(2, 1);
  
  // Complete pooling
  //-----------------
      for (g in 1:G) {

    omega[3:T_all, g] ~ normal(2 * omega[2:(T_all-1), g] - omega[1:(T_all-2), g], sigma_o);
    gamma[3:T_all, g] ~ normal(2 * gamma[2:(T_all-1), g] - gamma[1:(T_all-2), g], sigma_g);
    delta[3:T_all, g] ~ normal(2 * delta[2:(T_all-1), g] - delta[1:(T_all-2), g], sigma_d);
    alpha[3:T_all, g] ~ normal(2 * alpha[2:(T_all-1), g] - alpha[1:(T_all-2), g], sigma_a);

  }

  sigma_o ~ lognormal(-1.5, 0.5);
  sigma_g ~ lognormal(-1.5, 0.5);
  sigma_d ~ lognormal(-1.5, 0.5);
  sigma_a ~ lognormal(-1.5, 0.5);

}
