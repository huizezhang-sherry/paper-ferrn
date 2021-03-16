## ----interruption
interp <- before %>% get_interp()

p1_anno <- bind_rows(
  interp %>% filter(tries == 4, info == "interpolation") %>%
    filter(index_val == max(index_val)) %>%
    mutate(anno = "interpolation basis"),
  interp %>% get_interp_last %>% filter(tries %in% c(3, 4)) %>%
    mutate(anno = c("current basis", "target basis"))
) %>% arrange(id)

p1 <- before %>%
  explore_trace_interp(accuracy_y = 0.001) +
  scale_color_continuous_botanical() +
  geom_hline(data = get_best(after) %>% mutate(id = 78), aes(yintercept = index_val), color = "grey90") +
  ggrepel::geom_label_repel(data = p1_anno, aes(label = anno), box.padding = 0.5, alpha = 0.5) +
  ggtitle("without interruption")

p2 <- after %>%
  explore_trace_interp(accuracy_y = 0.001) +

  ggtitle("with interruption") +
  scale_color_continuous_botanical()

(p1 | p2)

## ----polish
# set.seed(123456)
# holes_2d_better <-
#   animate_xy(boa6, tour_path = guided_tour(holes(), d = 2,
#                                            search_f =  search_better, max.tries = 400),
#              rescale = FALSE)
# last_basis <- get_best(holes_2d_better)$basis %>% .[[1]]
# set.seed(123456)
# holes_2d_better_polish <-
#   animate_xy(boa6, tour_path = guided_tour(holes(), d = 2,
#                                            search_f =  search_polish),
#              rescale = FALSE, start = last_basis)
#
# save(holes_2d_better, file = here::here("data", "holes_2d_better.rda"))
# save(holes_2d_better_polish, file = here::here("data", "holes_2d_better_polish.rda"))
#
# set.seed(123456)
# render(
#   boa6,
#   tour_path = guided_tour(holes(), d = 2, search_f = search_better, max.tries = 400),
#   dev = "png",
#   display = display_xy(axes = "off", verbose = TRUE, col = botanical_palettes$fern[[1]]),
#   rescale = FALSE,
#   frames = 00,
#   file = here::here("anim","polish", "before%03d.png")
# )
# last_basis <- get_best(holes_2d_better)$basis %>% .[[1]]
#
# set.seed(123456)
# render(
#   boa6,
#   tour_path =  guided_tour(holes(), d = 2, search_f = search_polish),
#   dev = "png",
#   display = display_xy(axes = "off", verbose = TRUE, col = botanical_palettes$fern[[6]]),
#   start = last_basis,
#   rescale = FALSE,
#   frames = 100,
#   file = here::here("anim","polish", "after%03d.png")
# )

p1 <- bind_rows(holes_2d_better, holes_2d_better_polish) %>%
  clean_method() %>%
  mutate(method = factor(method, levels = c("CRS", "polish"))) %>%
  get_interp() %>%
  explore_trace_interp(color = method, cutoff = 100, target_size = 2, interp_size = 2) +
  scale_color_discrete_botanical(breaks = c("CRS", "polish"), label = c("CRS", "polish")) +
  theme(legend.position = "bottom")

wrap <- function(path) png::readPNG(path) %>% grid::rasterGrob() %>% wrap_plots()

first <- here::here("anim","polish", "before001.png")
before <- here::here("anim","polish", "before073.png")
after <- here::here("anim","polish", "after006.png")
file <- c(first, before, after)
rl <- lapply(file, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl) / p1 + plot_annotation(tag_levels = "A")

# p1 +
#   inset_element(grid::rasterGrob(before), left = 0.77, bottom = 0.45, right = 0.89, top = 0.67, align_to = "full") +
#   inset_element(grid::rasterGrob(after), left = 0.87, bottom = 0.7, right = 1, top = 1, align_to = "full")

## ---- flip-sign
# set.seed(2463)
# orientation_corrected <-
#   animate_dist(boa6,
#                tour_path = guided_tour(norm_kol(nrow(boa6)), d = 1,
#                                        search_f = search_better, max.tries = 200, 0.7),
#                rescale = FALSE)
#
# use ggobi/tourr version 0.6.0
# set.seed(2463)
# orientation_different <-
#   animate_dist(boa6,
#                tour_path = guided_tour(norm_kol(nrow(boa6)), d = 1,
#                                        search_f = search_better, max.tries = 200, 0.7),
#                rescale = FALSE)
#
# save(orientation_corrected, file = here::here("data", "orientation_corrected.rda"))
# save(orientation_different, file = here::here("data", "orientation_different.rda"))

dt <- bind_rows(orientation_corrected %>% mutate(sign = "flipped"),
          orientation_different %>% mutate(sign = "original")) %>%
  compute_pca() %>%
  pluck("aug")

dt %>%
  explore_space_pca(group = sign, pca = FALSE ,flip = FALSE, start_size = 3) +
  add_anchor(dt = get_interp_last(dt), anchor_color = sign) +
  scale_color_discrete_botanical() +
  facet_wrap(vars(fct_relevel(sign, c("original", "flipped")))) +
  theme(legend.position = "none")

## ---- noisy-better-geo
## code for generating kol_1d_geo, kol_1d_better and kol_1d_better_polish
# set.seed(123456)
# kol_1d_geo <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
#                                              search_f =  search_geodesic, max.tries = 200),
#                rescale = FALSE)
#
# set.seed(123456)
# kol_1d_better <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
#                                              search_f =  search_better, max.tries = 200),
#                rescale = FALSE)
#
# # last_basis <- get_best(kol_1d_better)$basis %>% .[[1]]
# #
# # set.seed(123456)
# # kol_1d_better_polish <-
# #   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
# #                                              search_f = search_polish, max.tries = 200),
# #                rescale = FALSE, start = last_basis)
#
# set.seed(123456)
# kol_1d_better_random <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
#                                              search_f =  search_better_random, max.tries = 200),
#                rescale = FALSE)
#
# # last_basis <- get_best(kol_1d_better_random)$basis %>% .[[1]]
# #
# # set.seed(123456)
# # kol_1d_better_random_polish <-
# #   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
# #                                              search_f =  search_polish, max.tries = 200),
# #                rescale = FALSE, start = last_basis)
#
# save(kol_1d_geo, file = here::here("data", "kol_1d_geo.rda"))
# save(kol_1d_better, file = here::here("data", "kol_1d_better.rda"))
# save(kol_1d_better_random, file = here::here("data", "kol_1d_better_random.rda"))


index <- tourr::norm_kol(nrow(boa5))
theo_best_index_val <- index(as.matrix(boa5) %*% matrix(c(0, 1, 0, 0, 0), nrow = 5, ncol = 1))

dt <- dplyr::bind_rows(kol_1d_geo, kol_1d_better, kol_1d_better_random) %>%
  bind_theoretical(matrix = matrix(c(0, 1, 0, 0, 0), ncol = 1), tourr::norm_kol(nrow(boa5)), raw_data = boa5)

p1 <- dt %>%
  explore_trace_interp(group = method, color = method, accuracy_y = 0.02) +
  geom_hline(yintercept = theo_best_index_val, alpha = 0.5, linetype = 2) +
  scale_color_discrete_botanical() +
  facet_wrap(vars(fct_relevel(method, c("PD", "CRS", "SA"))), scales = "free_x")

pca <- dt %>%
  compute_pca(group = method) %>%
  purrr::pluck("aug")

p2 <- pca %>%
  explore_space_pca(group = method, pca = FALSE, details = FALSE, start_size = 3, interp_size = 0.5, end_size = 5) +
  add_search(dt = pca %>% group_by(method) %>% filter(str_detect(info, "search")) %>% filter(tries != max(tries)),
             search_size = 0.5, search_col = method, search_alpha = 0.2) +
  add_dir_search(dt = pca %>% filter(method == "PD") %>% get_dir_search(ratio = 10) %>% filter(tries != max(tries)),
                 dir_col = method, dir_alpha = 0.2) +
  scale_color_discrete_botanical() +
  theme(legend.position = "none") +
  facet_wrap(vars(fct_relevel(method, c("PD", "CRS", "SA"))))

(p1 / p2)

