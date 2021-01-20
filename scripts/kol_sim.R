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
                   tour_path = guided_tour(norm_kol(nrow(boa6)), d = 1,
                                           search_f = search_f, max.tries = max.tries, alpha),
                   rescale = FALSE, verbose = TRUE) %>%
      mutate(seed = seed[i])

    last_basis <- get_best(optim_data[[i]])$basis %>% .[[1]]

    set.seed(seed[i])
    polish_data[[i]] <-
      animate_dist(boa6,
                   tour_path = guided_tour(norm_kol(nrow(boa6)), d= 1,
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

# clean up the four results
sim_clean_up <- function(sim_data) {
  sim_name <- rep(names(sim_data), each = 20)

  optim_tidy <- purrr::map(sim_data,  ~.x$optim_data) %>%
    unlist(recursive = FALSE) %>%
    map2(sim_name, ~.x %>% mutate(sim = .y)) %>%
    bind_rows()

  polish_tidy <- purrr::map(sim_data,  ~.x$polish_data) %>%
    unlist(recursive = FALSE) %>%
    map2(sim_name, ~.x %>% mutate(sim = .y)) %>%
    bind_rows()

  all_tidy <- bind_rows(optim_tidy, polish_tidy) %>% mutate(seed = as.factor(seed))

  all_tidy
}

find_best <- function(clean_up){
  names <- clean_up$sim %>% unique()
  best <- map_dfr(names, ~clean_up %>% filter(sim == .x) %>% get_best(seed)) %>%
    rename(index_val_best = index_val) %>%
    arrange(index_val_best) %>%
    mutate(var_found = map_dbl(basis, ~abs(.x) %>% which.max()),
           var_found = as.factor(ifelse(var_found == 2, "V2", "V7")))

  best
}

all_kol <- sim_clean_up(list(kol_better = kol_better,
                             kol_better_tuned = kol_better_tuned,
                             kol_random = kol_random,
                             kol_random_tuned= kol_random_tuned))

clean <- all_kol %>%
  left_join(find_best(all_kol) %>% select(seed, var_found, index_val_best, sim), by = c("seed", "sim") )

save(clean, file = here::here("data", "clean.rda"))
