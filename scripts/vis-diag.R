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
  scale_color_botanical(palette = "daisy")

p2 <- holes_2d_better_random %>%
  mutate(max_tries = 25) %>%
  explore_trace_search(label.size = 0.01, label.padding = 0.1, segment.size = 0, extend_lower = 0.93) +
  scale_color_botanical(palette = "daisy")

(p1 | p2)

## ---- toy-interp
p1 <- holes_2d_better_max_tries %>%
  mutate(group = "Algorithm 1") %>%
  explore_trace_interp(accuracy_x = 4) +
  scale_color_botanical(palette = "fern", discrete = FALSE)

p2 <- holes_2d_better_random %>%
  mutate(group = "Algorithm 3") %>%
  explore_trace_interp(accuracy_x = 14) +
  scale_color_botanical(palette = "fern", discrete = FALSE)

p1 | p2

## ---- toy-pca
bind_rows(holes_1d_geo, holes_1d_better) %>%
  bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
                   index = tourr::holes(), raw_data = boa5)  %>%
  explore_space_pca(group = method, interp_size = 1) +
  scale_color_botanical(palette = "fern")

## ----toy-pca-animated
# ani <- pca$aug %>%
#   ggplot(aes(x = PC1, y = PC2)) +
#   geom_point(data = pca$aug %>% filter(info == "randomly_generated"), col = "grey", size = 0.5) +
#   #geom_path(data =  pca$aug %>% filter(info == "interpolation"), aes(col = method), size = 2) +
#   geom_point(data =  pca$aug %>% filter(info != "randomly_generated"), aes(col = method), size = 5) +
#   geom_point(data = pca$aug %>% filter(info == "theoretical"), aes(col = method), size = 15) +
#   geom_point(data = get_start(pca$aug), aes(col = method), size = 10) +
#   scale_color_botanical(palette = "cherry") +
#   theme_void() +
#   theme(aspect.ratio = 1, legend.position =  "none",
#         panel.border = element_rect(colour = "black", fill=NA, size=1)) +
#   gganimate::transition_states(id) +
#   gganimate::shadow_mark()
#
# animate(ani, nframes = 350, device = "png",
#         renderer = file_renderer("anim/pca/",
#                                  prefix = "pca", overwrite = TRUE))

frames <- c("0002", "0058", "0078", "0133", "0172", "0350")
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

