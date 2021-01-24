## ----kol-result
clean_pca <- clean %>%
  filter(method != "search_polish") %>%
  bind_theoretical(matrix = matrix(c(0, 1, 0, 0, 0, 0), nrow = 6),
                   index = tourr::norm_kol(nrow(boa6)),
                   raw_data = boa6) %>%
  bind_theoretical(matrix = matrix(c(0, 0, 1, 0, 0, 0), nrow = 6),
                   index = tourr::norm_kol(nrow(boa6)),
                   raw_data = boa6) %>%
  mutate(seed_sim = paste0(seed, sim),
         alpha = ifelse(str_detect(sim, "tuned"), 0.7, 0.5),
         search_name = ifelse(str_detect(sim, "random"), "search_better_random", "search_better")) %>%
  compute_pca(group = seed_sim) %>%
  pluck("aug")

global_max <- clean_pca %>%
  filter(info == "theoretical") %>%
  filter(index_val == max(index_val)) %>%
  select(-alpha, -search_name)

local_max <- clean_pca %>%
  filter(info == "theoretical") %>%
  filter(index_val == min(index_val)) %>%
  select(-alpha, -search_name)

clean_pca %>% drop_na() %>%
  explore_space_pca(details = FALSE, pca = FALSE, group = seed_sim, color = var_found,
                    precision = 0.001,
                    start_size = 1, end_size = 2.5,
                    interp_size = 0.5) +
  add_theo(dt = global_max) +
  add_theo(dt = local_max, theo_label = "x", theo_size = 12) +
  scale_color_botanical() +
  facet_grid(alpha ~ search_name, labeller = label_both)




