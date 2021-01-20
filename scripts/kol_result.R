## ----kol-better
bind_boa6_theo <- function(dt){
  dt %>%
    bind_theoretical(matrix = matrix(c(0, 1, 0, 0, 0, 0), nrow = 6),
                     index = tourr::norm_kol(nrow(boa6)),
                     raw_data = boa6) %>%
    bind_theoretical(matrix = matrix(c(0, 0, 1, 0, 0, 0), nrow = 6),
                     index = tourr::norm_kol(nrow(boa6)),
                     raw_data = boa6)
}


clean %>%
  filter(sim == "kol_better",method != "search_polish") %>%
  bind_boa6_theo() %>%
  explore_space_pca(group = seed, color = var_found,
                    precision = 0.001, theo_size = 15,
                    start_size = 7) +
  scale_color_botanical() +
  facet_wrap(vars(fct_reorder(seed, index_val_best)))

# kol_better_tuned
kol_better_tuned <- clean %>%
  filter(sim == "kol_better_tuned", method != "search_polish") %>%
  bind_boa6_theo() %>%
  explore_space_pca(group = seed, color = var_found,
                    precision = 0.001, theo_size = 15,
                    start_size = 7) +
  scale_color_botanical() +
  facet_wrap(vars(fct_reorder(seed, index_val_best)))

## ----kol-random
clean %>%
  filter(sim == "kol_random",method != "search_polish") %>%
  bind_boa6_theo() %>%
  explore_space_pca(group = seed, color = var_found,
                    precision = 0.001, theo_size = 15,
                    start_size = 7) +
  scale_color_botanical() +
  facet_wrap(vars(fct_reorder(seed, index_val_best)))

## ----kol-random-tuned
clean %>%
  filter(sim == "kol_random_tuned", method != "search_polish") %>%
  bind_boa6_theo() %>%
  explore_space_pca(group = seed, color = var_found,
                    precision = 0.001, theo_size = 15,
                    start_size = 7) +
  scale_color_botanical() +
  facet_wrap(vars(fct_reorder(seed, index_val_best)))


