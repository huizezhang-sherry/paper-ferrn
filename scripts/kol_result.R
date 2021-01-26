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
         search_name = ifelse(str_detect(sim, "random"), "search_better_random", "search_better"),
         var_found = as.factor(ifelse(var_found == "V7", "global", "local")),
         var_found = fct_relevel(var_found, "global", after = 1)) %>%
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
  explore_space_pca(pca = FALSE, group = seed_sim, color = var_found,
                    start_size = 1, end_size = 2,
                    interp_size = 0.5) +
  add_theo(dt = global_max, theo_size = 20) +
  add_theo(dt = local_max, theo_label = "x", theo_size = 10) +
  scale_color_botanical() +
  facet_grid(alpha ~ search_name, labeller = label_both)

