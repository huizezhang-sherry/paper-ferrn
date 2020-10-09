## ---- load-pkg
library(ferrn)
library(tidyverse)
library(tourr)
library(patchwork)
library(ggrepel)
files <- paste0("data/", list.files(here::here("data")))
purrr::walk(.x = files, ~load(here::here(.x), env = globalenv()))

## ---- toy-search
bind_rows(holes_2d_better_max_tries %>% mutate(max_tries = 500),
          holes_2d_better %>% mutate(max_tries = 25)) %>%
  explore_trace_search(group = max_tries) +
  scale_color_botanical(palette = "daisy") +
  facet_wrap(vars(max_tries),labeller = "label_both", ncol = 2) +
  xlab("iteration")

## ---- toy-interp
bind_rows(holes_2d_better_max_tries %>% mutate(group = "Algorithm 1"),
          interrupt_no %>% mutate(group = "Algorithm 2"),
          holes_2d_better_random %>% mutate(group = "Algorithm 3")) %>%
  mutate(group = fct_relevel(group, c("Algorithm 1",
                                      "Algorithm 2",
                                      "Algorithm 3"))) %>%
  explore_trace_interp(group = group) +
  theme(legend.position = "none") +
  scale_color_botanical(palette = "fern")

## ---- toy-pca
bind_rows(holes_1d_geo, holes_1d_better) %>%
  bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
                   index = tourr::holes(), raw_data = boa5) %>%
  bind_theoretical(matrix(c(0, -1, 0, 0,0), nrow = 5),
                   index = tourr::holes(), raw_data  = boa5)%>%
  explore_space_pca(col = method)  +
  scale_color_botanical(palette = "cherry") +
  theme(legend.position = "bottom")

## ----toy-pca-animated
# ani <- bind_rows(holes_1d_geo, holes_1d_better) %>%
#   bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
#                    index = tourr::holes(), raw_data = data) %>%
#   bind_theoretical(matrix(c(0, -1, 0, 0,0), nrow = 5),
#                    index = tourr::holes(), raw_data  = data)%>%
#   explore_space_pca(col = method, animate = TRUE)  +
#   scale_color_botanical(palette = "cherry")
#
# animate(ani, nframes = 350, device = "png",
#         renderer = file_renderer("figures/pca/",
#                                  prefix = "pca", overwrite = TRUE))

frames <- c("0002", "0058", "0078", "0117", "0172", "0350")
ani <- paste0(here::here("anim/"), "pca/", "pca", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)

## ----toy-tour
set.seed(1)
sphere <- geozoo::sphere.hollow(p = 5, n = 1000)$point
path1 <- holes_1d_better$basis %>%
  flatten_dbl() %>% matrix(ncol = 5, byrow = TRUE)
path2 <- holes_1d_geo$basis %>%
  flatten_dbl() %>% matrix(ncol = 5, byrow = TRUE)
theoretical <- matrix(c(0, 1, 0, 0, 0,
                        0, -1, 0, 0, 0), nrow = 2, byrow = TRUE)
start <- get_start(holes_1d_better) %>% pull(basis) %>% .[[1]] %>% matrix(nrow = 1)
dt <- rbind(sphere, path1, path2, theoretical, start)
colnames(dt) <- c(map_chr(1:5, ~paste0("x", .x)))
pal <- c("#D3D3D3",c("#524340",  #orchre
                     "#B4B754",  # green
                     "#F3B422" # yellow
))
color <- c(rep(pal[1], nrow(sphere)),
           rep(pal[2], nrow(path1)),
           rep(pal[3], nrow(path2)),
           rep(pal[4], 2),
           pal[2] # for start
)
cex <- c(rep(1, nrow(sphere)),
         rep(1.5, nrow(path1)),
         rep(1.5, nrow(path2)),
         rep(5, 2),
         5 # for start
)

#
# bind_rows(holes_1d_geo, holes_1d_better) %>%
#   explore_space_tour(color = method)
#animate_xy(dt, col = color, cex = cex, tour_path = grand_tour()) # trial


# set.seed(123)
# render(
#   dt,
#   tour_path = grand_tour(),
#   dev = "png",
#   display = display_xy(col=color,cex = cex, axes = "bottomleft"),
#   rescale = FALSE,
#   frames = 500,
#   here::here("figures","tour", "tour%03d.png")
# )

frames <- c("001", "143", "078", "117", "172", "350")
ani <- paste0(here::here("anim/"), "tour/", "tour", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)

