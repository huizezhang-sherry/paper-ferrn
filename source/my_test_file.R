library(tidyverse)
library(tourr)
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

###############################
devtools::load_all()
set.seed(123456)
holes_1d_geo <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                   search_f =  tourr:::search_geodesic),
                     rescale = FALSE, verbose = TRUE)

devtools::load_all()
set.seed(123456)
holes_1d_geo_polish <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                                  search_f =  tourr:::search_polish),
                                    rescale = FALSE, verbose = TRUE, start = last_basis)



devtools::load_all()
set.seed(123456)
holes_1d_better <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                           search_f =  tourr:::search_better),
                             rescale = FALSE, verbose = TRUE)

############
devtools::load_all()
set.seed(123456)
holes_2d_geo <- animate_xy(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                             search_f =  tourr:::search_geodesic),
                           rescale = FALSE, verbose = TRUE)

last_basis <- holes_2d_geo %>% filter(info == "interpolation") %>% tail(1) %>% pull(basis) %>% .[[1]]

devtools::load_all()
set.seed(123456)
holes_2d_geo_polish <- animate_xy(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                                search_f =  tourr:::search_polish),
                              rescale = FALSE, verbose = TRUE, start = last_basis)

############
devtools::load_all()
set.seed(123456)
holes_2d_better <- animate_xy(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                             search_f =  tourr:::search_better),
                           rescale = FALSE, verbose = TRUE)

last_basis <- holes_2d_better %>% filter(info == "interpolation") %>% tail(1) %>% pull(basis) %>% .[[1]]

devtools::load_all()
set.seed(123456)
holes_2d_better_polish <- animate_xy(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                                    search_f =  tourr:::search_polish),
                                  rescale = FALSE, verbose = TRUE, start = last_basis)

############
devtools::load_all()
set.seed(123456)
holes_2d_better_max_tries <- animate_xy(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                                search_f =  tourr:::search_better, max.tries = 500),
                              rescale = FALSE, verbose = TRUE)

last_basis <- holes_2d_better_max_tries %>% filter(info == "interpolation") %>% tail(1) %>% pull(basis) %>% .[[1]]

devtools::load_all()
set.seed(123456)
holes_2d_better_max_tries_polish <- animate_xy(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                                       search_f =  tourr:::search_polish),
                                     rescale = FALSE, verbose = TRUE, start = last_basis)

############
devtools::load_all()
set.seed(123456)
holes_2d_pos <- animate_xy(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                             search_f = search_posse, max.tries = 500),
                           rescale = FALSE, verbose = TRUE)

last_basis <- holes_2d_pos %>% filter(info == "interpolation") %>% tail(1) %>% pull(basis) %>% .[[1]]

devtools::load_all()
set.seed(123456)
holes_2d_pos_polish <- animate_xy(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                                       search_f =  tourr:::search_polish),
                                     rescale = FALSE, verbose = TRUE, start = last_basis)

############
devtools::load_all()
set.seed(123456)
holes_2d_better_random <- animate_xy(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                             search_f = tourr:::search_better_random, max.tries = 1000),
                           rescale = FALSE, verbose = TRUE)

last_basis <- holes_2d_better_random %>% filter(info == "interpolation") %>% tail(1) %>% pull(basis) %>% .[[1]]

devtools::load_all()
set.seed(123456)
holes_2d_better_random_polish <- animate_xy(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                                    search_f =  tourr::: search_polish),
                                  rescale = FALSE, verbose = TRUE, start = last_basis)

###############################
save(holes_1d_geo, file = "source/test/holes_1d_geo.rda")
save(holes_1d_geo_polish, file = "source/test/holes_1d_geo_polish.rda")
save(holes_1d_better, file = "source/test/holes_1d_better.rda")
save(holes_2d_geo, file = "source/test/holes_2d_geo.rda")
save(holes_2d_geo_polish, file = "source/test/holes_2d_geo_polish.rda")
save(holes_2d_better, file = "source/test/holes_2d_better.rda")
save(holes_2d_better_polish, file = "source/test/holes_2d_better_polish.rda")
save(holes_2d_better_max_tries, file = "source/test/holes_2d_better_max_tries.rda")
save(holes_2d_better_max_tries_polish, file = "source/test/holes_2d_better_max_tries_polish.rda")
save(holes_2d_better_random, file = "source/test/holes_2d_better_random.rda")
save(holes_2d_better_random_polish, file = "source/test/holes_2d_better_random_polish.rda")
save(holes_2d_pos, file = "source/test/holes_2d_pos.rda")
save(holes_2d_pos_polish, file = "source/test/holes_2d_pos_polish.rda")
