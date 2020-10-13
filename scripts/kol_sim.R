compute_kol_sim <- function(optim_data, polish_data, search_f, alpha = 0.5, max.tries = 200){
  #browser()
  set.seed(123)
  seed <- sample.int(10000, 20)
  optim_data <- list()
  polish_data <- list()

  for (i in 1: length(seed)){
    cat("i = ", i, "seed = ", seed[i], "\n")
    set.seed(seed[i])
    optim_data[[i]] <-
      animate_dist(boa6,
                   tour_path = guided_tour(norm_kol(), d = 1,
                                           search_f = search_f, max.tries = max.tries, alpha),
                   rescale = FALSE, verbose = TRUE) %>%
      mutate(seed = seed[i])

    last_basis <- get_best(optim_data[[i]])$basis %>% .[[1]]

    set.seed(seed[i])
    polish_data[[i]] <-
      animate_dist(boa6,
                   tour_path = guided_tour(norm_kol(), d= 1,
                                           search_f = search_polish, max.tries = 500),
                   rescale = FALSE, verbose = TRUE,
                   start = last_basis) %>%
      mutate(seed = seed[i])
  }

  list(optim_data = optim_data, polish_data = polish_data)
}

kol_better <- compute_kol_sim(kol_better_optim, kol_better_polish, search_f = search_better)
kol_better_tuned <- compute_kol_sim(kol_better_optim_tuned, kol_better_polish_tuned, search_f = search_better, alpha = 0.7)
kol_random <- compute_kol_sim(kol_random_optim, kol_random_polish, search_f = search_better_random)
kol_random_tuned <- compute_kol_sim(kol_random_optim_tuned, kol_random_polish_tuned, search_f = search_better_random, alpha = 0.7)


save(kol_better, file = here::here("data", "kol_better.rda"))
save(kol_better_tuned, file = here::here("data", "kol_better_tuned.rda"))
save(kol_random, file = here::here("data", "kol_random.rda"))
save(kol_random_tuned, file = here::here("data", "kol_random_tuned.rda"))


