library(tidyverse)
set.seed(1234)
x1 <- rnorm(1000, 0, 1)
x2 <- sample(c(rnorm(500, -3, 1), rnorm(500, 3, 1)), size = 1000)
x3 <- sample(c(rep(-1, 500), rep(1, 500)), size = 1000)
x4 <- sample(c(rnorm(250, -3, 1), rnorm(750, 3, 1)), size = 1000)
x5 <- sample(c(rnorm(330, -5, 1), rnorm(340, 0, 1), rnorm(330, 5, 1)), size = 1000)
x6 <- sample(c(rnorm(450, -5, 1), rnorm(100, 0, 1), rnorm(450, 5, 1)), size = 1000)
x7 <- sample(c(rnorm(500, -5, 1), rnorm(500, 5, 1)), size = 1000)
x8 <- rnorm(1000, 0, 1)
x9 <- rnorm(1000, 0, 1)
x10 <- rnorm(1000, 0, 1)

data <- tibble::tibble(x1 = x1, x2 = x2, x8 = x8,
                       x9 = x9, x10 = x10) %>% purrr::map_df(scale)

data_mult <- tibble::tibble(x1 = x1, x2 = x2, x3 = x3,
                            x4 = x4, x5 = x5, x6 = x6, x7 = x7,
                            x8 = x8, x9 = x9, x10 = x10) %>% map_df(scale)

