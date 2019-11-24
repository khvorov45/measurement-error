# Simulations of data ideal for the models
# Arseniy Khvorov
# Created 2019/10/30
# Last edit 2019/11/07

suppressPackageStartupMessages(library(rstan))
suppressPackageStartupMessages(library(tidyverse))
library(purrr)
library(ggdark) # devtools::install("khvorov45/ggdark")
library(future)
library(furrr)

rstan_options(auto_write = TRUE)
plan(multiprocess) # May not work on windows

model_folder <- "model"
sim_folder <- "sim"

# Functions ===================================================================

sim_linear <- function(n = 100, beta0 = -1, betax = 1, sigma = 1,
                       x_error = 0, y_error = 0,
                       seed = sample.int(.Machine$integer.max, 1)) {
  set.seed(seed)
  tibble(.rows = n) %>%
    mutate(
      x_true = rnorm(n(), 0, 5),
      y_true = rnorm(n(), beta0 + betax * x_true, sigma),
      x_meas = rnorm(n(), x_true, x_error),
      y_meas = rnorm(n(), y_true, y_error)
    )
}

sim_props <- function(n = 100, rel_fit = 1, kappa = 5,
                      x_error = 0, y_error = 0,
                      seed = sample.int(.Machine$integer.max, 1)) {
  set.seed(seed)
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

sim_data_dict <- function(fun, data_name, dict,
                          seed = sample.int(.Machine$integer.max, 1)) {
  pars <- dict %>% filter(name == data_name) %>% select(-name)
  do.call(fun, c(pars, seed = seed))
}



fit_stan_model <- function(model_compiled, data, iter = 2000,
                           seed = sample.int(.Machine$integer.max, 1)) {
  sampling(
    model_compiled,
    data = list(
      n = nrow(data),
      y = data$y_meas,
      x = data$x_meas
    ),
    cores = 1,
    chains = 2,
    iter = iter,
    seed = seed
  )
}

tidy_stan_fit <- function(stan_fit) {
  pars <- names(stan_fit)
  pars <- pars[!grepl("\\[", pars)]
  pars <- pars[pars != "lp__"]
  summ <- summary(stan_fit, pars = pars)$summary
  summ %>%
    as_tibble() %>%
    mutate(
      seed = get_seed(stan_fit),
      term = rownames(summ),
      iter = length(rstan::extract(stan_fit, pars = pars)[[1]])
    ) %>%
    select(term, everything())
}

sim_fit_one <- function(fun, data_name, data_dict,
                        model_compiled, iter,
                        seed = sample.int(.Machine$integer.max, 1)) {
  dat <- sim_data_dict(fun, data_name, data_dict, seed)
  true_vals <- data_dict %>%
    filter(name == data_name) %>%
    select(-name) %>%
    pivot_longer(everything(), names_to = "term", values_to = "true_val")
  fit_stan_model(model_compiled, dat, iter, seed) %>%
    tidy_stan_fit() %>%
    left_join(true_vals, by = "term")
}

sim_fit_many <- function(nsim, init_seed, ...) {
  future_map_dfr(1:nsim, function(i) sim_fit_one(..., seed = init_seed + i))
}

save_res <- function(res, nsim, data_name, model_name, sim_folder) {
  res %>%
    mutate(data = data_name, model = model_name) %>%
    write_csv(
      file.path(
        sim_folder,
        paste0(data_name, "--", model_name, "--", nsim, "sims.csv")
      )
    )
}

# Script ======================================================================

data_dict_lin <- tribble(
  ~name, ~n, ~beta0, ~betax, ~sigma, ~x_error, ~y_error,
  "med-pos-noerror", 200, -1, 1, 1, 0, 0,
  "med-pos-xyerror", 200, -1, 1, 1, 0.5, 0.5,
)

sim_fun <- sim_linear
data_name <- "med-pos-noerror"
data_dict <- data_dict_lin
nsim <- 2
model_name <- "linear"
model_compiled <- stan_model(
  file.path(model_folder, paste0(model_name, ".stan"))
)

res <- sim_fit_many(
  nsim, 20191107, sim_fun, data_name, data_dict, model_compiled, 40000
)

save_res(res, nsim, data_name, model_name, sim_folder)
