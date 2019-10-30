// Linear model with measurement error on x
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
  vector[n] true_x;
}

model {
  true_x ~ normal(0, 1);
  x ~ normal(true_x, 0.5);
  beta0 ~ normal(0, 10);
  betax ~ normal(0, 10);
  sigma ~ exponential(0.1);
  y ~ normal(beta0 + betax * true_x, sigma);
}
