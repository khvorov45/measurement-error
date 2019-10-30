// Linear model with measurement error on x and y
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
  vector[n] true_y;
}

model {
  true_y ~ normal(0, 5);
  y ~ normal(true_y, 0.5);
  true_x ~ normal(0, 1);
  x ~ normal(true_x, 0.5);
  beta0 ~ normal(0, 10);
  betax ~ normal(0, 10);
  sigma ~ exponential(0.1);
  true_y ~ normal(beta0 + betax * true_x, sigma);
}
