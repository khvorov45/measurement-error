# Simulations of data ideal for the models
# Arseniy Khvorov
# Created 2019/10/30
# Last edit 2019/10/30

suppressPackageStartupMessages(library(rstan))
suppressPackageStartupMessages(library(tidyverse))
library(purrr)
library(ggdark) # devtools::install("khvorov45/ggdark")

model_folder <- "model"

sim_linear <- function(n, beta0, betax, sigma, x_error = 0, y_error = 0) {
  tibble(.rows = n) %>%
    mutate(
      x_true = rnorm(n()),
      y_true = rnorm(n(), beta0 + betax * x_true, sigma),
      x_meas = rnorm(n(), x_true, x_error),
      y_meas = rnorm(n(), y_true, y_error)
    )
}

sim_props <- function(n, rel_fit, kappa, x_error = 0, y_error = 0) {
  rbeta_prop <- function(n, mu, kappa) rbeta(n, mu * kappa, (1 - mu) * kappa)
  prop_error <- function(vec, err) {
    if (err == 0) return(vec)
    truncnorm::rtruncnorm(length(vec), 0, 1, vec, err)
  }
  tibble(.rows = n) %>%
    mutate(
      x_true = runif(n(), 0, 1),
      y_true = rbeta_prop(
        n(), x_true / (x_true + (1 - x_true) * exp(rel_fit)), kappa
      ),
      x_meas = prop_error(x_true, x_error),
      y_meas = prop_error(y_true, y_error)
    )
}

plot_scatter <- function(lin) {
  lin %>%
    ggplot(aes(x_meas, y_meas)) +
    dark_theme_bw(verbose = FALSE) +
    geom_point()
}

plot_x_error <- function(lin) {
  lin %>%
    ggplot(aes(x_meas, x_true)) +
    dark_theme_bw(verbose = FALSE) +
    geom_point()
}

plot_y_error <- function(lin) {
  lin %>%
    ggplot(aes(y_meas, y_true)) +
    dark_theme_bw(verbose = FALSE) +
    geom_point()
}

fit_stan_model <- function(model_compiled, data) {
  sampling(
    model_compiled,
    data = list(
      n = nrow(data),
      y = data$y_meas,
      x = data$x_meas
    ),
    cores = 1,
    chains = 1,
    iter = 2000
  )
}

lin <- stan_model(file.path(model_folder, "linear.stan"))
lin_xme <- stan_model(file.path(model_folder, "linear-xme.stan"))
lin_xyme <- stan_model(file.path(model_folder, "linear-xyme.stan"))

props <- stan_model(file.path(model_folder, "props.stan"))
props_xme <- stan_model(file.path(model_folder, "props-xme.stan"))
props_xyme <- stan_model(file.path(model_folder, "props-xyme.stan"))

dat <- sim_props(1e3, 1, 5, 0.05, 0.05)
plot_scatter(dat)
plot_x_error(dat)
plot_y_error(dat)

fit <- fit_stan_model(props_xyme, dat)
summary(fit, pars = c("rel_fit", "kappa"))$summary
