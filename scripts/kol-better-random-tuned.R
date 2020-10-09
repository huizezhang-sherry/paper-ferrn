## ---- kol-better-random-tuned
set.seed(123)
seed <- sample.int(10000, 20)
# kol_1d_2var_better_random_sim <- list()
# kol_1d_2var_better_random_sim_polish <- list()
# for (i in 1: length(seed)){
#   cat("i = ", i, "seed = ", seed[i], "\n")
#   set.seed(seed[i])
#   kol_1d_2var_better_random_sim[[i]] <-
#     animate_dist(boa6,
#                  tour_path = guided_tour(norm_kol(), d = 1,
#                                          search_f = search_better_random, max.tries = 500, t0 = 1e-6, alpha = 0.7),
#                  rescale = FALSE, verbose = TRUE)
#
#
#   last_basis <- get_best(kol_1d_2var_better_random_sim[[i]])$basis %>% .[[1]]
#
#   set.seed(seed[i])
#   kol_1d_2var_better_random_sim_polish[[i]] <-
#     animate_dist(boa6,
#                  tour_path = guided_tour(norm_kol(), d = 1,
#                                          search_f = search_polish, max.tries = 200),
#                  rescale = FALSE, verbose = TRUE,
#                  start = last_basis)
#
# }


#save(kol_1d_2var_better_random_sim, file = here::here("data", "kol_1d_2var_better_random_sim.rda"))
#save(kol_1d_2var_better_random_sim_polish, file = here::here("data", "kol_1d_2var_better_random_sim_polish.rda"))


polish_list <- purrr::map2(kol_1d_2var_better_random_sim_polish,seed,  ~.x %>% dplyr::mutate(seed = .y))
better_list <- purrr::map2(kol_1d_2var_better_random_sim,seed,  ~.x %>% dplyr::mutate(seed = .y))
polish_tidy <- do.call(dplyr::bind_rows, polish_list)
better_tidy <- do.call(dplyr::bind_rows, better_list)
all_tidy <- bind_rows(better_tidy, polish_tidy)

best <- get_best(polish_tidy, group = seed) %>% arrange(index_val)
best <- get_best(better_tidy, group = seed) %>% arrange(index_val)
best <- get_best(all_tidy, group = seed) %>% arrange(index_val)

all_summary <- best$basis %>%
  purrr::flatten_dbl() %>% matrix(ncol = 6, byrow = TRUE) %>%
  as_tibble() %>%
  rename("V7" = "V3", "V8" = "V4", "V9" = "V5", "V10" = "V6") %>%
  mutate(IndexVal = best$index_val, seed = as.factor(best$seed)) %>%
  pivot_longer(-c(seed, IndexVal), names_to = "VarFound", values_to = "value") %>%
  group_by(seed, IndexVal) %>%
  filter(abs(value) == max(abs(value))) %>%
  arrange(IndexVal) %>% ungroup()

pca <- all_tidy %>%
  mutate(seed = as.factor(seed)) %>%
  left_join(all_summary %>% select(seed, VarFound, IndexVal), by = "seed") %>%
  bind_theoretical(matrix = matrix(c(0, 1, 0, 0, 0, 0), nrow = 6),
                   index = tourr::norm_kol(),
                   raw_data = boa6) %>%
  bind_theoretical(matrix = matrix(c(0, 0, 1, 0, 0, 0), nrow = 6),
                   index = tourr::norm_kol(),
                   raw_data = boa6) %>%

  compute_pca()

random <- pca$aug %>% filter(method == "randomly_generated") %>% select(-seed)
theoretical <- pca$aug %>% filter(info == "theoretical") %>% select(-seed)

pca$aug %>% filter(method != "randomly_generated" & info != "theoretical") %>%
  group_by(seed) %>% arrange(IndexVal) %>%
  ggplot(aes(x = PC1, y = PC2, col = VarFound)) +
  geom_point(data = random, col = "grey", size = 0.3) +
  geom_point(aes(col = VarFound), alpha = 0.05) +
  geom_point(data = theoretical, size = 5, col = botanical_palettes$cherry[3]) +
  geom_path(data = pca$aug %>% filter(info == "interpolation"), size = 2) +
  geom_point(data = pca$aug %>% get_best(group = seed), size = 5) +
  facet_wrap(vars(fct_reorder(seed, IndexVal))) +
  theme_void() +
  theme(aspect.ratio = 1, legend.position = "bottom") +
  scale_color_botanical(palette = "fern")
