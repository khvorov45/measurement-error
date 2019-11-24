// Linear model with no measurement error
// Arseniy Khvorov
// Created 2019/10/30
// Last edit 2019/10/30

data {
  int<lower=1> n;
  vector[n] y;
  vector[n] x;
  int<lower=0> n_xmiss;
  int<lower=1> x_miss_ind[n_xmiss];
}

parameters {
  real beta0;
  real betax;
  real<lower=0> sigma;
  vector[n_xmiss] x_imp;
}

transformed parameters {
  vector[n] x_reconst;
  x_reconst = x;
  x_reconst[x_miss_ind] = x_imp;
}

model {
  beta0 ~ normal(0, 10);
  betax ~ normal(0, 10);
  sigma ~ exponential(0.1);
  x_imp ~ normal(0, 5);
  y ~ normal(beta0 + betax * x_reconst, sigma);
}
