# Simulations of data ideal for the models
# Arseniy Khvorov
# Created 2019/10/30
# Last edit 2019/11/07

suppressPackageStartupMessages(library(rstan))
suppressPackageStartupMessages(library(tidyverse))
library(purrr)
library(future)
library(furrr)
library(truncnorm)
suppressPackageStartupMessages(library(extraDistr))

rstan_options(auto_write = TRUE)
plan(multiprocess) # May not work on windows

model_dir <- "model"
sim_dir <- "sim"

# Functions ===================================================================

sim_linear <- function(n = 100, beta0 = -2, betax = 2, sigma = 1,
                       x_error = 0, y_error = 0, x_miss = 0, y_miss = 0,
                       seed = sample.int(.Machine$integer.max, 1)) {
  set.seed(seed)
  tibble(
    x_true = rnorm(n, 0, 5),
    y_true = rnorm(n, beta0 + betax * x_true, sigma),
    x_meas = rnorm(n, x_true, x_error),
    y_meas = rnorm(n, y_true, y_error),
    x_miss = rbern(n, x_miss),
    y_miss = rbern(n, y_miss),
    x_obs = if_else(as.logical(x_miss), NA_real_, x_meas),
    y_obs = if_else(as.logical(y_miss), NA_real_, y_meas)
  )
}

sim_props <- function(n = 100, rel_fit = 1, kappa = 5,
                      x_error = 0, y_error = 0,
                      seed = sample.int(.Machine$integer.max, 1)) {
  set.seed(seed)
  rbeta_prop <- function(n, mu, kappa) rbeta(n, mu * kappa, (1 - mu) * kappa)
  prop_error <- function(vec, err) {
    if (err == 0) return(vec)
    rtruncnorm(length(vec), 0, 1, vec, err)
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

sim_bin <- function(n, beta0, betax, x_miss, y_miss,
                   seed = sample.int(.Machine$integer.max, 1)) {
  tibble(
    x_true = rnorm(n, 2, 2),
    y_true = rbern(n, 1 - 1 / (1 + exp(beta0 + betax * x_true))),
    x_miss = rbern(n, x_miss),
    y_miss = rbern(n, y_miss),
    x_obs = if_else(as.logical(x_miss), NA_real_, x_true),
    y_obs = if_else(as.logical(y_miss), NA_real_, y_true)
  )
}

sim_data_dict <- function(fun, data_name, dict,
                          seed = sample.int(.Machine$integer.max, 1)) {
  if (!data_name %in% dict$name) rlang::abort("unrecognised data name")
  pars <- dict %>% filter(name == data_name) %>% select(-name)
  do.call(fun, c(pars, seed = seed))
}

get_data_list <- function(model_name, sim_data) {
  if (model_name %in% c("linear", "binary"))
    data_list <- list(
      n = nrow(sim_data),
      x = sim_data$x_obs,
      y = sim_data$y_obs
    )
  else if (grepl("^linear-.*miss$", model_name))
    data_list <- list(
      n = nrow(sim_data),
      x = if_else(is.na(sim_data$x_obs), -1e6, sim_data$x_obs),
      y = if_else(is.na(sim_data$y_obs), -1e6, sim_data$y_obs),
      n_xmiss = sum(is.na(sim_data$x_obs)),
      x_miss_ind = which(is.na(sim_data$x_obs)),
      n_ymiss = sum(is.na(sim_data$y_obs)),
      y_miss_ind = which(is.na(sim_data$y_obs))
    )
  else if (model_name == "binary-ymiss") {
    data_list <- list(
      n = nrow(sim_data),
      x = sim_data$x_obs,
      y = if_else(is.na(sim_data$y_obs), -1, sim_data$y_obs),
      y_miss = as.integer(is.na(sim_data$y_obs))
    )
  }
  else rlang::abort("unrecognised model name")
  data_list
}

fit_stan_model <- function(model_compiled, data, iter = 2000,
                           seed = sample.int(.Machine$integer.max, 1)) {
  sampling(
    model_compiled,
    data = get_data_list(model_compiled@model_name, data),
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

sim_fit_many <- function(nsim, fun, data_name, data_dict,
                         model_compiled, iter,
                         init_seed = sample.int(.Machine$integer.max, 1)) {
  future_map_dfr(
    1:nsim, function(i) sim_fit_one(
      fun, data_name, data_dict, model_compiled, iter,
      seed = init_seed + i
    )
  ) %>% mutate(data = data_name, model = model_compiled@model_name)
}

save_res <- function(res, nsim, sim_folder) {
  data_name <- unique(res$data)
  model_name <- unique(res$model)
  res %>%
    write_csv(
      file.path(
        sim_folder,
        paste0(data_name, "--", model_name, "--", nsim, "sims.csv")
      )
    )
}

# Script ======================================================================

nsim <- 2

# Linear data

data_dict_lin <- tribble(
  ~name, ~n, ~beta0, ~betax, ~sigma, ~x_error, ~y_error, ~x_miss, ~y_miss,
  "med-pos-noerror", 200, -1, 1, 1, 0, 0, 0, 0,
  "med-pos-xyerror", 200, -1, 1, 1, 0.5, 0.5, 0, 0,
  "med-pos-xmiss", 200, -1, 1, 1, 0, 0, 0.1, 0,
  "med-pos-xymiss", 200, -1, 1, 1, 0, 0, 0.1, 0.1
)

# Binary data

data_dict_bin <- tribble(
  ~name, ~n, ~beta0, ~betax, ~x_miss, ~y_miss,
  "med-pos-nomiss", 200, -5, 1.5, 0, 0,
  "med-pos-ymiss", 200, -5, 1.5, 0, 0.1
)

# No error, simple linear model

# linear_comp <- stan_model(file.path(model_dir, "linear.stan"))
# res <- sim_fit_many(
#   nsim, sim_linear,
#   "med-pos-noerror", data_dict_lin, linear_comp, 40000, 20191107
# )
# save_res(res, nsim, sim_folder)

# No error, x missing, linear model

# linear_xmiss_comp <- stan_model(file.path(model_dir, "linear-xmiss.stan"))
# res <- sim_fit_many(
#   nsim, sim_linear,
#   "med-pos-xmiss", data_dict_lin, linear_xmiss_comp, 40000, 20191107
# )
# save_res(res, nsim, sim_folder)

# No error, x and y missing, linear model

# linear_xymiss_comp <- stan_model(file.path(model_dir, "linear-xymiss.stan"))
# res_xymiss <- sim_fit_many(
#   nsim, sim_linear,
#   "med-pos-xymiss", data_dict_lin, linear_xymiss_comp, 40000, 20191107
# )
# save_res(res_xymiss, nsim, sim_folder)

# Simple binary data
# binary_comp <- stan_model(file.path(model_dir, "binary.stan"))
# res_bin <- sim_fit_many(
#   nsim, sim_bin,
#   "med-pos-nomiss", data_dict_bin, binary_comp, 40000, 20191107
# )
# save_res(res_bin, nsim, sim_folder)

# Binary data with y missing
binary_ymiss_comp <- stan_model(file.path(model_dir, "binary-ymiss.stan"))
res_bin_ymiss <- sim_fit_many(
  nsim, sim_bin,
  "med-pos-ymiss", data_dict_bin, binary_ymiss_comp, 40000, 20191107
)
save_res(res_bin_ymiss, nsim, sim_folder)

