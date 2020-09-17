library(tidyverse)
library(tourr)
library(ferrn)
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
                       x9 = x9, x10 = x10) %>% scale() %>% as_tibble()

data_mult <- tibble::tibble(x1 = x1, x2 = x2, x7 = x7,
                            x8 = x8, x9 = x9, x10 = x10) %>% scale() %>% as_tibble()

get_alpha <- function(dt){
  last <- dt %>% filter(info == "interpolation") %>% tail(1) %>% pull(index_val) %>% .[[1]]
  second_last <- dt %>% filter(info == "interpolation") %>% tail(2) %>% pull(index_val) %>% .[[1]]

  last - second_last
}

save(data_mult, file = here::here("data", "data_mult.rda"))
save(data, file = here::here("data", "data.rda"))


################################################
# polish example
set.seed(123456)
holes_2d_better <- animate_xy(data_mult, tour_path = guided_tour(holes(), d = 2,
                                                                 search_f =  search_better),
                              rescale = FALSE, verbose = TRUE)

set.seed(123456)
holes_2d_better_polish <- animate_xy(data_mult, tour_path = guided_tour(holes(), d = 2,
                                                                        search_f =  search_polish,
                                                                        alpha = get_alpha(holes_2d_better)),
                                     rescale = FALSE, verbose = TRUE, start = get_best(holes_2d_better)$basis)

###
set.seed(123456)
holes_2d_better_max_tries <- animate_xy(data_mult, tour_path = guided_tour(holes(), d = 2,
                                                                           search_f =  search_better, max.tries = 500),
                                        rescale = FALSE, verbose = TRUE)

set.seed(123456)
holes_2d_better_max_tries_polish <- animate_xy(data_mult, tour_path = guided_tour(holes(), d = 2,
                                                                                  search_f =  search_polish,
                                                                                  alpha = get_alpha(holes_2d_better_max_tries)),
                                               rescale = FALSE, verbose = TRUE, start = get_best(holes_2d_better_max_tries)$basis)

basis <- orthonormalise(matrix(c(0, 0, -1, 0, 0, 0,
                                 0, 1, 0, 0, 0, 0), ncol = 2))

index_f <- holes()
index_val <- index_f(as.matrix(data_mult) %*% basis)

holes_2d_theory <- tibble(basis = list(basis),
                          index_val = index_val,
                          info = "theory")

save(holes_2d_theory, file = here::here("data", "holes_2d_theory.rda"))
save(holes_2d_better, file = here::here("data", "holes_2d_better.rda"))
save(holes_2d_better_polish, file = here::here("data", "holes_2d_better_polish.rda"))
save(holes_2d_better_max_tries, file = here::here("data", "holes_2d_better_max_tries.rda"))
save(holes_2d_better_max_tries_polish, file = here::here("data", "holes_2d_better_max_tries_polish.rda"))

################################################
# polish_alpha example - this part has problem!

set.seed(123456)
holes_1d_geo <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                           search_f =  search_geodesic),
                             rescale = FALSE, verbose = TRUE)

set.seed(123456)
holes_1d_geo_polish <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                                  search_f =  search_polish,
                                                                  alpha = get_alpha(holes_1d_geo)),
                                    rescale = FALSE, verbose = TRUE, start = get_best(holes_1d_geo)$basis)

holes_1d_geo_polish_default_alpha <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                                                search_f =  search_polish),
                                                  rescale = FALSE, verbose = TRUE, start = get_best(holes_1d_geo)$basis)


################################################
# noisy index
# 1) search_geodesic vs. search_better - the polish part has problem!
set.seed(123456)
kol_1d_geo <- animate_dist(data, tour_path = guided_tour(norm_kol(), d = 1,
                                                           search_f =  search_geodesic),
                             rescale = FALSE, verbose = TRUE)


set.seed(123456)
kol_1d_geo_polish <- animate_dist(data, tour_path = guided_tour(norm_kol(), d = 1,
                                                                search_f =  search_polish,
                                                                alpha = get_alpha(kol_1d_geo)),
                                    rescale = FALSE, verbose = TRUE, start = get_best(kol_1d_geo)$basis)

set.seed(123456)
kol_1d_better <- animate_dist(data, tour_path = guided_tour(norm_kol(), d = 1,
                                                              search_f =  search_better),
                                rescale = FALSE, verbose = TRUE)

set.seed(123456)
kol_1d_better_polish <- animate_dist(data, tour_path = guided_tour(norm_kol(), d = 1,
                                                                search_f =  search_polish,
                                                                alpha = get_alpha(kol_1d_better)),
                                  rescale = FALSE, verbose = TRUE, start = get_best(kol_1d_better)$basis)

save(kol_1d_geo, file = here::here("data", "kol_1d_geo.rda"))
#save(kol_1d_geo_polish, file = "data", "kol_1d_geo_polish.rda")
save(kol_1d_better, file = here::here("data", "kol_1d_better.rda"))
#save(kol_1d_better_polish, file = "data", "kol_1d_better_polish.rda")

# 2) search_better different seeds and search_better_random - polish has problem!
set.seed(123456)
kol_1d_2var_better <- animate_dist(data_mult, tour_path = guided_tour(norm_kol(), d = 1,
                                                                      search_f = search_better, max.tries = 1000),
                                       rescale = FALSE, verbose = TRUE)

set.seed(123456)
kol_1d_2var_better_polish <- animate_dist(data_mult, tour_path = guided_tour(norm_kol(), d = 1,
                                                                             search_f =  search_polish),
                                              rescale = FALSE, verbose = TRUE, start = get_last_basis(kol_1d_2var_better))

set.seed(12345)
kol_1d_2var_better_2 <- animate_dist(data_mult, tour_path = guided_tour(norm_kol(), d = 1,
                                                                        search_f = search_better, max.tries = 5000),
                                         rescale = FALSE, verbose = TRUE)

set.seed(123456)
kol_1d_2var_better_2_polish <- animate_dist(data_mult, tour_path = guided_tour(norm_kol(), d = 1,
                                                                               search_f =  search_polish),
                                                rescale = FALSE, verbose = TRUE, start = get_last_basis(kol_1d_2var_better_2))

set.seed(12345)
kol_1d_2var_better_random <- animate_dist(data_mult, tour_path = guided_tour(norm_kol(), d = 1,
                                                                      search_f = search_better_random,
                                                                      max.tries = 5000, t0 = 0.02),
                                              rescale = FALSE, verbose = TRUE)

set.seed(123456)
kol_1d_2var_better_random_polish <- animate_dist(data_mult[,c(1,2, 7:10)], tour_path = guided_tour(norm_kol(), d = 1,
                                                                                                       search_f =  search_polish),
                                                     rescale = FALSE, verbose = TRUE, start = get_last_basis(kol_1d_2var_better_random))

save(kol_1d_2var_better, file = here::here("data", "kol_1d_2var_better.rda"))
#save(kol_1d_2var_better_polish, file = here::here("data", "kol_1d_2var_better_polish.rda"))
save(kol_1d_2var_better_2, file = here::here("data", "kol_1d_2var_better_2.rda"))
#save(kol_1d_2var_better_2_polish, file = here::here("data", "kol_1d_2var_better_2_polish.rda"))
save(kol_1d_2var_better_random, file = here::here("data", "kol_1d_2var_better_random.rda"))
#save(kol_1d_2var_better_random_polish, file = here::here("data", "kol_1d_2var_better_random_polish.rda"))


################################################
# have not yet run!
# other simulations

set.seed(123456)
holes_2d_better_random <- animate_xy(data_mult, tour_path = guided_tour(holes(), d = 2,
                                                                        search_f = search_better_random, max.tries = 1000),
                                     rescale = FALSE, verbose = TRUE)

set.seed(123456)
holes_2d_better_random_polish <- animate_xy(data_mult, tour_path = guided_tour(holes(), d = 2,
                                                                               search_f =  search_polish,
                                                                               alpha = get_alpha(holes_2d_better_random)),
                                            rescale = FALSE, verbose = TRUE, start = get_last_basis(holes_2d_better_random))

save(holes_2d_better_random, file = here::here("data", "holes_2d_better_random.rda"))

################################################
set.seed(123456)
holes_1d_geo <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                           search_f =  search_geodesic),
                             rescale = FALSE, verbose = TRUE)

set.seed(123456)
holes_1d_geo_polish <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                                  search_f =  search_polish,
                                                                  alpha = get_alpha(holes_1d_geo)),
                                    rescale = FALSE, verbose = TRUE, start = get_last_basis(holes_1d_geo))

save(holes_1d_geo, file = here::here("data", "holes_1d_geo.rda"))

################################################
set.seed(123456)
holes_1d_better <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                              search_f =  search_better),
                                rescale = FALSE, verbose = TRUE)

save(holes_1d_better, file = here::here("data", "holes_1d_better.rda"))
################################################
# code for saving
