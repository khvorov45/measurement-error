// Proportions model with measurement error on x
// Arseniy Khvorov
// Created 2019/10/30
// Last edit 2019/10/30

data {
  int<lower=1> n;
  vector<lower=0,upper=1>[n] y;
  vector<lower=0,upper=1>[n] x;
}

parameters {
  real rel_fit;
  real<lower=0> kappa;
  vector<lower=0,upper=1>[n] x_true;
  vector<lower=0,upper=1>[n] y_true;
}

transformed parameters {
  vector<lower=0,upper=1>[n] y_expec;
  for (i in 1:n) {
    y_expec[i] = x_true[i] / (x_true[i] + (1 - x_true[i]) * exp(rel_fit));
  }
}

model {
  x_true ~ uniform(0, 1);
  x ~ normal(x_true, 0.05);
  y_true ~ uniform(0, 1);
  y ~ normal(y_true, 0.05);
  rel_fit ~ normal(0, 3);
  kappa ~ exponential(0.1);
  y_true ~ beta_proportion(y_expec, kappa);
}
