## ----interruption
p1_anno <- interrupt_no %>% filter(info == "interpolation") %>% mutate(id = row_number()) %>% filter(id %in% c(44, 60, 62)) %>%
  mutate(anno = c("current basis", "interpolated basis", "target basis"))

p1 <- interrupt_no %>% mutate(id = row_number() - 1) %>% explore_trace_interp() + ggtitle("without interruption") +
  geom_point(data = p1_anno) +
  geom_label_repel(data = p1_anno, aes(label = anno), box.padding = 0.5) + ylim(0.8, 0.9) + xlim(0, 80) +
  theme(legend.position = "none")

p2 <- interrupt_yes %>% explore_trace_interp() + ggtitle("with interruption") + ylim(0.8, 0.9) + xlim(0, 80) +
  theme(legend.position = "none")

#(p1 | p2) & scale_color_botanical(palette = "fern", discrete = TRUE)

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
# set.seed(123456)
# kol_1d_geo <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(), d = 1,
#                                              search_f =  search_geodesic, max.tries = 100),
#                rescale = FALSE, verbose = TRUE)
#
# set.seed(123456)
# kol_1d_better <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(), d = 1,
#                                              search_f =  search_better, max.tries = 100),
#                rescale = FALSE, verbose = TRUE)
#
# last <- get_best(kol_1d_better)$basis %>% .[[1]]
#
# set.seed(123456)
# kol_1d_better_polish <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(), d = 1,
#                                              search_f =  search_polish, max.tries = 100),
#                rescale = FALSE, verbose = TRUE, start = last)
#
#
# save(kol_1d_geo, file = here::here("data", "kol_1d_geo.rda"))
# save(kol_1d_better, file = here::here("data", "kol_1d_better.rda"))
# save(kol_1d_better_polish, file = here::here("data", "kol_1d_better_polish.rda"))

index <- tourr::norm_kol(nrow(boa5))
theo_best_index_val <- index(as.matrix(boa5) %*% matrix(c(0, 1, 0, 0, 0), nrow = 5, ncol = 1))


p1 <- kol_1d_geo %>%
  explore_trace_interp() +
  geom_hline(yintercept = theo_best_index_val, alpha = 0.5, linetype = 2) +
  scale_color_botanical(discrete = FALSE)

p2 <- kol_1d_better %>%
  explore_trace_interp() +
  geom_hline(yintercept = theo_best_index_val, alpha = 0.5, linetype = 2) +
  scale_color_botanical(discrete = FALSE)

p1 | p2
