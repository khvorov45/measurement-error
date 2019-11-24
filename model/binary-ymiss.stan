// Binary model with no missing value handling
// Arseniy Khvorov
// Created 2019/11/22
// Last edit 2019/11/25

data {
  int<lower=1> n;
  int<lower=-1,upper=1> y[n];
  vector[n] x;
  int<lower=0,upper=1> y_miss[n];
}

parameters {
  real beta0;
  real betax;
}

model {
  beta0 ~ normal(0, 100);
  betax ~ normal(0, 100);
  for (i in 1:n) {
    if (y_miss[i] == 1) {
      target += log_mix(
        inv_logit(beta0 + betax * x[i]),
        bernoulli_logit_lpmf(1 | beta0 + betax * x[i]),
        bernoulli_logit_lpmf(0 | beta0 + betax * x[i])
      );
    } else {
      y[i] ~ bernoulli_logit(beta0 + betax * x[i]);
    }
  }
}
