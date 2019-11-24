# Testing sim functions
# Arseniy Khvorov
# Created 2019/11/25
# Last edit 2019/11/25

library(ggdark) # devtools::install_github("khvorov45/ggdark")

plot_scatter <- function(dat) {
  dat %>%
    ggplot(aes(x_obs, y_obs)) +
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

sim_data_dict(sim_linear, "med-pos-xymiss", data_dict_lin) %>%
  plot_scatter()
