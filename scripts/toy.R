## ---- load-pkg
library(ferrn)
library(tidyverse)
library(tourr)
library(gganimate)
library(patchwork)
library(ggrepel)
files <- paste0("data/", list.files(here::here("data")))
purrr::walk(.x = files, ~load(here::here(.x), env = globalenv()))

## ---- toy-search
p1 <- holes_2d_better_max_tries %>%
  mutate(max_tries = 500) %>%
  explore_trace_search(label.size = 0.03,label.padding = 0.1, segment.size = 0,  extend_lower = 0.93) +
  scale_color_botanical(palette = "fern")

p2 <- holes_2d_better_random %>%
  mutate(max_tries = 25) %>%
  explore_trace_search(label.size = 0.01, label.padding = 0.1, segment.size = 0, extend_lower = 0.93) +
  scale_color_botanical(palette = "fern")

(p1 | p2) & theme_bw() + theme(legend.position = "none")

## ---- toy-interp
p1 <- holes_2d_better_max_tries %>%
  mutate(group = "Algorithm 1") %>%
  explore_trace_interp(iter = id, color = tries, accuracy_x = 4) +
  scale_color_botanical(palette = "fern", discrete = FALSE)

p2 <- holes_2d_better_random %>%
  mutate(group = "Algorithm 3") %>%
  explore_trace_interp(iter = id, color = tries, accuracy_x = 14) +
  scale_color_botanical(palette = "fern", discrete = FALSE)

p1 | p2

## ---- toy-pca
bind_rows(holes_1d_geo, holes_1d_better) %>%
  bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
                   index = tourr::holes(), raw_data = boa5)  %>%
  explore_space_pca(group = method, interp_size = 1, anchor_size = 2, finish_size = 2, ratio = 4.5) +
  scale_color_botanical(palette = "fern")

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
# set.seed(1)
# sphere <- geozoo::sphere.hollow(p = 5, n = 1000)$point
# path1 <- holes_1d_better$basis %>%
#   flatten_dbl() %>% matrix(ncol = 5, byrow = TRUE)
# path2 <- holes_1d_geo$basis %>%
#   flatten_dbl() %>% matrix(ncol = 5, byrow = TRUE) %>% -.
# theoretical <- matrix(c(0, 1, 0, 0, 0), nrow = 1, byrow = TRUE)
# start <- get_start(holes_1d_better) %>% pull(basis) %>% .[[1]] %>% matrix(nrow = 1)
# dt <- rbind(sphere, path1, path2, theoretical, start, -start)
# colnames(dt) <- c(map_chr(1:5, ~paste0("x", .x)))
# pal <- c("#D3D3D3",c("#524340",  #orchre
#                      "#B4B754",  # green
#                      "#F3B422" # yellow
# ))
# color <- c(rep(pal[1], nrow(sphere)),
#            rep(pal[2], nrow(path1)),
#            rep(pal[3], nrow(path2)),
#            rep(pal[4], 1), # theoretical
#            pal[2], # for start
#            pal[3] # for start
# )
# cex <- c(rep(1, nrow(sphere)),
#          rep(3, nrow(path1)),
#          rep(3, nrow(path2)),
#          10, # theoretical
#          rep(5, 2) # for start
# )
#
# animate_xy(dt, col = color, cex = cex, tour_path = grand_tour()) # trial
#
# set.seed(123)
# render(
#   dt,
#   tour_path = grand_tour(),
#   dev = "png",
#   display = display_xy(col=color,cex = cex, axes = "bottomleft"),
#   rescale = FALSE,
#   frames = 150,
#   here::here("anim","tour", "tour%03d.png")
# )

frames <- c("001", "020", "078", "117", "123", "143")
ani <- paste0(here::here("anim/"), "tour/", "tour", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)

