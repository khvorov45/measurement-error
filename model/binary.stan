// Binary model with no missing value handling
// Arseniy Khvorov
// Created 2019/11/22
// Last edit 2019/11/25

data {
  int<lower=1> n;
  int<lower=0,upper=1> y[n];
  vector[n] x;
}

parameters {
  real beta0;
  real betax;
}

model {
  beta0 ~ normal(0, 100);
  betax ~ normal(0, 100);
  y ~ bernoulli_logit(beta0 + betax * x);
}
