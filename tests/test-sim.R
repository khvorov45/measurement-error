# Testing sim functions
# Arseniy Khvorov
# Created 2019/11/25
# Last edit 2019/11/25

plot_scatter <- function(dat) {
  dat %>%
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
