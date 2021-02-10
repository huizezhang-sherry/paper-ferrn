## ---- load-pkg
library(ferrn)
library(tidyverse)
library(patchwork)
library(tourr)
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
                    interp_size = 1, ratio = 9) +
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

frames <- c("0001", "0038", "0046", "0079", "0086", "0100")
ani <- paste0(here::here("anim/"), "pca/", "pca", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)

## ----toy-tour
# prep <- prep_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo), flip = TRUE,
#                         group = method, palette = botanical_palettes$fern[c(1,6)], axes = "bottomleft",
#                         point_size = 3, end_size = 8)
#
# set.seed(123456)
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
# set.seed(123456)
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

frames <- c("001", "021", "056", "072", "079", "088")
ani <- paste0(here::here("anim/"), "tour/", "tour", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)

## ----toy-torus
# proj_d <-  2 # 2D because then you have 2 orthogonal circles
# d <- 6
# n_point <- 5000
# index <- holes()
# set.seed(123456)
# random <- map(1:n_point, ~basis_random(n = d/proj_d,  d=proj_d)) %>%
#   purrr::flatten_dbl() %>% matrix(ncol = d, byrow = TRUE) %>% as_tibble()
#
# set.seed(123456)
# holes_2d_geo_3var <-
#   animate_xy(boa6[, 1:3], tour_path = guided_tour(holes(), d = 2,
#                                            search_f =  search_geodesic),
#              rescale = FALSE)
#
# last <- get_best(holes_2d_geo_3var)$basis %>% .[[1]]
#
# set.seed(123456)
# holes_2d_geo_3var_polish <-
#   animate_xy(boa6[, 1:3], tour_path = guided_tour(holes(), d = 2,
#                                                   search_f =  search_polish,
#                                                   start = last),
#              rescale = FALSE)
#
# set.seed(123456)
# holes_2d_better_3var <-
#   animate_xy(boa6[, 1:3], tour_path = guided_tour(holes(), d = 2,
#                                                   search_f =  search_better),
#              rescale = FALSE)
#
# last <- get_best(holes_2d_better_3var)$basis %>% .[[1]]
#
# set.seed(123456)
# holes_2d_better_3var_polish <-
#   animate_xy(boa6[, 1:3], tour_path = guided_tour(holes(), d = 2,
#                                                   search_f =  search_polish),
#              rescale = FALSE, start = last)
#
# save(holes_2d_better_3var, file = here::here("data", "holes_2d_better_3var.rda"))
# save(holes_2d_geo_3var, file = here::here("data", "holes_2d_geo_3var.rda"))
#
#
# path_geo <- bind_rows(holes_2d_geo_3var) %>% get_interp() %>% get_basis_matrix() %>% as_tibble()
# path_better <- bind_rows(holes_2d_better_3var) %>% get_interp() %>% get_basis_matrix() %>% as_tibble()
#
# path_better <- holes_2d_better_3var %>% group_by(tries) %>% filter(loop != max(loop)) %>%get_interp() %>% get_basis_matrix() %>% as_tibble()
#
#
# basis <- bind_rows(path_geo, path_better, random)  %>%
#   mutate(id = as.factor(ifelse(row_number() > nrow(path_geo) + nrow(path_better), "random",
#                      ifelse(row_number() <= nrow(path_geo), "geodesic", "better"))),
#          cex = ifelse(id == "random", 0.5, 3),
#          cex = ifelse(row_number() == 1, 5, cex)) %>%
#   group_by(id) %>%
#   mutate(cex = ifelse(row_number() == max(row_number()) & id != "random",
#                       7, cex)) %>%
#   ungroup()
#
# pal <- RColorBrewer::brewer.pal(3, "Dark2")
# pal <- c(botanical_pal(reverse = TRUE)(2), "grey60")
# col <- pal[basis$id]
#
# set.seed(123)
# render(
#   basis[,1:d],
#   tour_path = grand_tour(),
#   dev = "png",
#   display = display_xy(cex = basis$cex, col = col, axes = "off"),
#   rescale = FALSE,
#   frames = 100,
#   here::here("anim","torus", "torus%03d.png")
# )
#
# # render gif
# set.seed(123)
# render_gif(
#   basis[,1:d],
#   tour_path = grand_tour(),
#   display = display_xy(cex = basis$cex ,col = col,  axes = "off"),
#   rescale = FALSE,
#   frames = 100,
#   gif_file = here::here("anim","torus.gif")
# )

frames <- c("001", "017", "064", "068", "075", "100")
ani <- paste0(here::here("anim/"), "torus/", "torus", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)

#explore_space_pca(bind_rows(holes_2d_better_3var, holes_2d_geo_3var), group = method)
