## ----kol-better
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
           var_found = ifelse(var_found == 2, "V2", "V7"))

  best
}


sim_pca <- function(clean_up){

  best <- find_best(clean_up)

  pca <- clean_up %>%
    bind_theoretical(matrix = matrix(c(0, 1, 0, 0, 0, 0), nrow = 6),
                     index = tourr::norm_kol(),
                     raw_data = boa6) %>%
    bind_theoretical(matrix = matrix(c(0, 0, 1, 0, 0, 0), nrow = 6),
                     index = tourr::norm_kol(),
                     raw_data = boa6) %>%
    mutate(group_id = paste0(sim, seed)) %>%
    compute_pca(group = group_id) %>%
    .$aug %>%
    left_join(best %>% select(index_val_best, var_found, seed, sim), by = c("sim", "seed"))

  pca
}

sim_make_plot <- function(pca){
  search <- pca %>% filter(info %in% c("new_basis", "interpolation", "random_search"))
  random <- pca %>% filter(info == "randomly_generated") %>% select(basis: PC2) %>% select(-seed)
  theoretical <- pca %>% filter(info == "theoretical") %>%  select(basis: PC6) %>% select(-seed)

  search %>%
    ggplot(aes(x = PC1, y = PC2, col = var_found)) +
    geom_point(data = random, col = "grey", size = 0.3, alpha = 0.5) +
    geom_point(data = theoretical, col = botanical_palettes$cherry[3], size = 5) +
    geom_point(data = search %>% filter(info != "interpolation"), size = 0.5, alpha = 0.5) +
    geom_path(data = search %>% filter(info == "interpolation"), size = 2) +
    geom_point(data = search %>% get_best(group = seed) %>% filter(info != "theoretical"), size = 5) +
    geom_point(data = search %>% get_start() %>% filter(method != "search_polish"), size = 3) +
    facet_wrap(vars(fct_reorder(seed, index_val_best))) +
    theme_void() +
    theme(aspect.ratio = 1, legend.position = "bottom") +
    scale_color_botanical(palette = "fern")

}

find_theo <- function(pca){
  pca %>% filter(info == "theoretical") %>%  select(basis: PC2) %>% select(-seed)
}


best <- sim_clean_up(list(kol_better = kol_better,
                          kol_better_tuned = kol_better_tuned,
                          kol_random = kol_random,
                          kol_random_tuned = kol_random_tuned))%>%
  find_best()

pca <- sim_clean_up(list(kol_better = kol_better,
                  kol_better_tuned = kol_better_tuned,
                  kol_random = kol_random,
                  kol_random_tuned= kol_random_tuned))%>%
  sim_pca()

pca %>% filter(sim == "kol_better" | info %in% c("randomly_generated", "theoretical")) %>% sim_make_plot()
#p2 <- pca %>% filter(sim == "kol_better_tuned" | info %in% c("randomly_generated", "theoretical")) %>% sim_make_plot()

## ----kol-random
pca %>% filter(sim == "kol_random" | info %in% c("randomly_generated", "theoretical")) %>% sim_make_plot()

## ----kol-random-tuned
pca %>% filter(sim == "kol_random_tuned" | info %in% c("randomly_generated", "theoretical")) %>% sim_make_plot()
