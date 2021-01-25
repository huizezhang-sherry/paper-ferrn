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
  explore_trace_interp(iter = id, color = tries, accuracy_y = 0.001) +
  scale_color_botanical(discrete = FALSE) +
  ggtitle("without interruption") +
  geom_point(data = p1_anno, size = 3) +
  geom_point(data = get_best(after) %>% mutate(id = 78), size = 0, color = "white") +
  geom_label_repel(data = p1_anno, aes(label = anno), box.padding = 0.5, alpha = 0.5)

p2 <- after %>%
  explore_trace_interp(iter = id, color = tries, accuracy_y = 0.001) +
  ggtitle("with interruption") +
  scale_color_botanical(discrete = FALSE)

(p1 | p2)

## ----polish
# set.seed(123456)
# render(
#   boa6,
#   tour_path = guided_tour(holes(), d = 2, search_f = search_geodesic),
#   dev = "png",
#   display = display_xy(axes = "bottomleft", verbose = TRUE),
#   rescale = FALSE,
#   frames = 100,
#   file = here::here("anim","polish", "before%03d.png")
# )
#
# last_basis <- get_best(holes_2d_geo)$basis %>% .[[1]]
#
# set.seed(123456)
# render(
#   boa6,
#   tour_path =  guided_tour(holes(), d = 2, search_f = search_polish, alpha = 0.1),
#   dev = "png",
#   display = display_xy(axes = "bottomleft", verbose = TRUE),
#   start = last_basis,
#   rescale = FALSE,
#   frames = 100,
#   file = here::here("anim","polish", "after%03d.png")
# )

# set.seed(123456)
# holes_2d_geo <-
#   animate_xy(boa6, tour_path = guided_tour(holes(), d = 2,
#                                            search_f =  search_geodesic),
#                            rescale = FALSE, verbose = TRUE)
#
# last_basis <- get_best(holes_2d_geo)$basis %>% .[[1]]
#
# set.seed(123456)
# holes_2d_geo_polish <-
#   animate_xy(boa6, tour_path = guided_tour(holes(), d = 2,
#                                            search_f =  search_polish),
#                                   rescale = FALSE, verbose = TRUE, start = last_basis)
# save(holes_2d_geo, file = here::here("data", "holes_2d_geo.rda"))
#save(holes_2d_geo_polish, file = here::here("data", "holes_2d_geo_polish.rda"))

before <- png::readPNG(here::here("anim","polish", "before100.png"))
after <- png::readPNG(here::here("anim","polish", "after004.png"))
gl <-  lapply(list(before, after), grid::rasterGrob)
wrap_plots(gl)

## ---- noisy-better-geo
## code for generating kol_1d_geo, kol_1d_better and kol_1d_better_polish
# set.seed(123456)
# kol_1d_geo <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
#                                              search_f =  search_geodesic, max.tries = 200),
#                rescale = FALSE, verbose = TRUE)
#
# set.seed(123456)
# kol_1d_better <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
#                                              search_f =  search_better, max.tries = 200),
#                rescale = FALSE, verbose = TRUE)
#
# last <- get_best(kol_1d_better)$basis %>% .[[1]]
#
# set.seed(123456)
# kol_1d_better_polish <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
#                                              search_f =  search_polish, max.tries = 100),
#                rescale = FALSE, verbose = TRUE, start = last)
#
#
# save(kol_1d_geo, file = here::here("data", "kol_1d_geo.rda"))
# save(kol_1d_better, file = here::here("data", "kol_1d_better.rda"))
# save(kol_1d_better_polish, file = here::here("data", "kol_1d_better_polish.rda"))


index <- tourr::norm_kol(nrow(boa5))
theo_best_index_val <- index(as.matrix(boa5) %*% matrix(c(0, 1, 0, 0, 0), nrow = 5, ncol = 1))

dt <- dplyr::bind_rows(kol_1d_geo, kol_1d_better) %>%
  bind_theoretical(matrix = matrix(c(0, -1, 0, 0, 0), ncol = 1), tourr::norm_kol(nrow(boa5)), raw_data = boa5)

p1 <- dt %>%
  explore_trace_interp(color = method, iter = id,group = method) +
  geom_hline(yintercept = theo_best_index_val, alpha = 0.5, linetype = 2) +
  scale_color_botanical() +
  facet_wrap(vars(method), scales = "free_x")

pca <- dt %>%
  compute_pca(group = method, flip = FALSE) %>%
  purrr::pluck("aug")

p2 <- pca %>%
  explore_space_pca(group = method, pca = FALSE, details = FALSE, start_size = 1) +
  scale_color_botanical() +
  theme(legend.position = "none")

(p1 | p2)

