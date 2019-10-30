// Linear model with no measurement error
// Arseniy Khvorov
// Created 2019/10/30
// Last edit 2019/10/30

data {
  int<lower=1> n;
  vector[n] y;
  vector[n] x;
}

parameters {
  real beta0;
  real betax;
  real<lower=0> sigma;
}

model {
  beta0 ~ normal(0, 10);
  betax ~ normal(0, 10);
  sigma ~ exponential(0.1);
  y ~ normal(beta0 + betax * x, sigma);
}
