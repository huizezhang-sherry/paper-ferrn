## ---- load-pkg
library(ferrn)
library(tidyverse)
library(patchwork)
files <- paste0("data/", list.files(here::here("data")))
purrr::walk(.x = files, ~load(here::here(.x), env = globalenv()))

## ---- toy-search
p1 <- holes_2d_better_max_tries %>%
  mutate(max_tries = 500) %>%
  explore_trace_search(label.size = 0.03,label.padding = 0.1, segment.size = 0,  extend_lower = 0.93) +
  scale_color_continuous_botanical()

p2 <- holes_2d_better_random %>%
  mutate(max_tries = 25) %>%
  explore_trace_search(label.size = 0.01, label.padding = 0.1, segment.size = 0, extend_lower = 0.93) +
  scale_color_continuous_botanical()

(p1 | p2) & theme_bw() + theme(legend.position = "none")

## ---- toy-interp
p1 <- holes_2d_better_max_tries %>%
  mutate(group = "Algorithm 1") %>%
  explore_trace_interp(accuracy_x = 4) +
  scale_color_continuous_botanical()

p2 <- holes_2d_better_random %>%
  mutate(group = "Algorithm 3") %>%
  explore_trace_interp( accuracy_x = 14) +
  scale_color_continuous_botanical()

p1 | p2

## ---- toy-pca
bind_rows(holes_1d_geo, holes_1d_better) %>%
  bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
                   index = tourr::holes(), raw_data = boa5)  %>%
  explore_space_pca(group = method, details = TRUE,
                    interp_size = 1, ratio = 7, dir_alpha = 0.5) +
  scale_color_discrete_botanical()

## ----toy-pca-animated
# dt <- dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>%
#   bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
#                    index = tourr::holes(), raw_data = boa5)
# ani <- dt %>%
#   explore_space_pca(group = method, animate = TRUE, interp_size = 3,
#                     theo_size = 45, start_size = 10, end_size = 20) +
#   theme(legend.position = "none") +
#   scale_color_botanical(palette = "fern")
# animate(ani, nframes = 100, device = "png",
#         renderer = file_renderer("anim/pca/",
#                                  prefix = "pca", overwrite = TRUE))

# vimeo version with legend
# ani <- dt %>%
#   explore_space_pca(group = method, animate = TRUE, interp_size = 3,
#                     theo_size = 45, start_size = 10, end_size = 20) +
#   scale_color_botanical(palette = "fern")
# anim_save(ani, filename = here::here("anim/toy-pca-animated.mp4"))

frames <- c("0001", "0038", "0046", "0079", "0096", "0100")
ani <- paste0(here::here("anim/"), "pca/", "pca", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)

## ----toy-tour
# prep <- prep_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo) %>%
#                      bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
#                                       index = tourr::holes(), raw_data = boa5),
#                    group = method, palette = botanical_palettes$fern[c(1,6)], axes = "bottomleft",
#                    point_size = 3, end_size = 8, theo_size = 8)
#
# set.seed(123)
# render(
#   prep$basis,
#   tour_path = grand_tour(),
#   dev = "png",
#   display = display_xy(col = prep$col, cex = prep$cex, pch = prep$pch,
#                        edges = prep$edges, edges.col = prep$edges_col,
#                        axes = "bottomleft"),
#   rescale = FALSE,
#   frames = 100,
#   here::here("anim","tour", "tour%03d.png")
# )
#
# # render gif
# set.seed(123)
# render_gif(
#   prep$basis,
#   tour_path = grand_tour(),
#   display = display_xy(col = prep$col, cex = prep$cex, pch = prep$pch,
#                        edges = prep$edges, edges.col = prep$edges_col,
#                        axes = "bottomleft"),
#   rescale = FALSE,
#   frames = 100,
#   gif_file = here::here("anim","tour.gif")
# )

frames <- c("001", "038", "046", "079", "096", "100")
ani <- paste0(here::here("anim/"), "tour/", "tour", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)

