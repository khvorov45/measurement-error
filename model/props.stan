// Proportions model with no measurement error
// Arseniy Khvorov
// Created 2019/10/30
// Last edit 2019/10/30

functions {
  // Stan's beta distribution definition sets density to 0 at 1 and 0
  // and disallows 1 and 0 as parameters.
  vector remove_proportion_bounds(vector vec, int n) {
    vector[n] out;
    for (i in 1:n) {
      if (vec[i] == 1) out[i] = 1 - 1e-6;
      else if (vec[i] == 0) out[i] = 1e-6;
      else out[i] = vec[i];
    }
    return out;
  }
}

data {
  int<lower=1> n;
  vector<lower=0,upper=1>[n] y;
  vector<lower=0,upper=1>[n] x;
}

transformed data {
  vector<lower=0,upper=1>[n] y_mod;
  vector<lower=0,upper=1>[n] x_mod;
  y_mod = remove_proportion_bounds(y, n);
  x_mod = remove_proportion_bounds(x, n);
}

parameters {
  real rel_fit;
  real<lower=0> kappa;
}

transformed parameters {
  vector<lower=0,upper=1>[n] y_expec;
  for (i in 1:n) {
    y_expec[i] = x_mod[i] / (x_mod[i] + (1 - x_mod[i]) * exp(rel_fit));
  }
}

model {
  rel_fit ~ normal(0, 3);
  kappa ~ exponential(0.1);
  y_mod ~ beta_proportion(y_expec, kappa);
}
