library(tidyverse)
library(geozoo)
library(tourr)
pca <- rbind(holes_1d_better,
             holes_1d_geo) %>%
  compute_pca()

pca$aug %>%
  ggplot(aes(x = PC1, y = PC2, col = as.factor(method))) +
  geom_point() +
  theme(aspect.ratio = 1)

# 1) only sphere
set.seed(1)
sphere <- geozoo::sphere.hollow(p = 5, n = 1000)$point
dt <- sphere
colnames(dt) <- c(map_chr(1:5, ~paste0("x", .x)))
pal <- c("#1B9E77","#D95F02","#7570B3", "#E7298A")
color <- c(rep(pal[1], nrow(sphere)))
animate_xy(dt, col = color, cex_slice = 0.8)
animate_slice(dt, col = color, cex_slice = 0.8)

# 2) sphere and  holes_1d_geo
path1 <- holes_1d_geo$basis %>% flatten_dbl() %>% matrix(ncol = 5, byrow = TRUE)
dt <- rbind(sphere, path1)
colnames(dt) <- c(map_chr(1:5, ~paste0("x", .x)))
pal <- c("#1B9E77","#D95F02","#7570B3", "#E7298A")
color <- c(rep(pal[1], nrow(sphere)),
           rep(pal[2], nrow(path1)))
animate_xy(dt, col = color, cex_slice = 0.8)
animate_slice(dt, col = color, cex_slice = 0.8)

# 3) sphere and holes_1d_better
path2 <- holes_1d_better$basis %>% flatten_dbl() %>% matrix(ncol = 5, byrow = TRUE)
dt <- rbind(sphere, path2)
colnames(dt) <- c(map_chr(1:5, ~paste0("x", .x)))
pal <- c("#1B9E77","#D95F02","#7570B3", "#E7298A")
color <- c(rep(pal[1], nrow(sphere)),
           rep(pal[3], nrow(path2)))
animate_xy(dt, col = color)
animate_slice(dt, col = color, cex_slice = 1, cex_other = 1)

# 4) sphere,  holes_1d_geo and holes_1d_better
path1 <- holes_1d_geo$basis %>% flatten_dbl() %>% matrix(ncol = 5, byrow = TRUE)
path2 <- holes_1d_better$basis %>% flatten_dbl() %>% matrix(ncol = 5, byrow = TRUE)
dt <- rbind(sphere, path1, path2)
colnames(dt) <- c(map_chr(1:5, ~paste0("x", .x)))
pal <- c("#1B9E77","#D95F02","#7570B3", "#E7298A")
color <- c(rep(pal[1], nrow(sphere)),
           rep(pal[2], nrow(path1)),
           rep(pal[3], nrow(path2)))
animate_xy(dt, col = color, cex_slice = 0.8)
animate_slice(dt, col = color)

dt <- rbind(sphere, path1, path2)
pca <- dt %>% prcomp()
summary(pca)
p1 <- predict(pca) %>%
  as_tibble() %>%
  mutate(method = c(rep("sphere", nrow(sphere)),
                    holes_1d_geo$method,
                    holes_1d_better$method)) %>%
  ggplot(aes(x = PC1, y = PC2, color = method)) +
  geom_point() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Dark2")

# PCA gives therpojection that maximises the varaince
# hence can be interpreted as the spread of search on the high dimensional sphere
# search_geodesic is having a much larger

# it is one view from the animated high diemnsional sphere, being punched to 2D

# because the space that the projection basis lives is a high dimensional sphere
# slice tour works as a display that highlights the points outside the "selection" when it is on the rim
# in this case would be easier to visualise than the grand tour
# because the grand tour does't allow you to see the hollow structure of the sphere
# another way to say is that slice tour helps to preceive the hollow sphere better than grand tour


dt <- rbind(sphere, path2)
pca <- dt %>% prcomp()
summary(pca)
p2 <- predict(pca) %>%
  as_tibble() %>%
  mutate(method = c(rep("sphere", nrow(sphere)),
                    holes_1d_better$method)) %>%
  ggplot(aes(x = -PC2, y = PC1, color = method)) +
  geom_point() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Dark2")


dt <- rbind(sphere, path1)
pca <- dt %>% prcomp()
summary(pca)
p3 <- predict(pca) %>%
  as_tibble() %>%
  mutate(method = c(rep("sphere", nrow(sphere)),
                    holes_1d_geo$method)) %>%
  ggplot(aes(x = PC1, y = PC2, color = method)) +
  geom_point() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Dark2" )

(p1 | p2 | p3) & theme(legend.position = "bottom")


####### 2D
# 1) only sphere
set.seed(1)
sphere <- geozoo::sphere.hollow(p = 12, n = 1000)$point
dt <- sphere
colnames(dt) <- c(map_chr(1:12, ~paste0("x", .x)))
pal <- c("#1B9E77","#D95F02","#7570B3", "#E7298A")
color <- c(rep(pal[1], nrow(sphere)))
animate_xy(dt, col = color, cex_slice = 0.8)
animate_slice(dt, col = color, cex_slice = 0.8)

# 2) sphere and  holes_2d_better
path1 <- holes_2d_better$basis %>% flatten_dbl() %>% matrix(ncol = 6, byrow = TRUE)
dt <- rbind(sphere, path1)
colnames(dt) <- c(map_chr(1:6, ~paste0("x", .x)))
pal <- c("#1B9E77","#D95F02","#7570B3", "#E7298A")
color <- c(rep(pal[1], nrow(sphere)),
           rep(pal[2], nrow(path1)))
animate_xy(dt, col = color, cex_slice = 0.8)
animate_slice(dt, col = color, cex_slice = 0.8)

# 3) sphere and holes_2d_better_max_tries
path2 <- holes_2d_better_max_tries$basis %>% flatten_dbl() %>% matrix(ncol = 6, byrow = TRUE)
dt <- rbind(sphere, path2)
colnames(dt) <- c(map_chr(1:6, ~paste0("x", .x)))
pal <- c("#1B9E77","#D95F02","#7570B3", "#E7298A")
color <- c(rep(pal[1], nrow(sphere)),
           rep(pal[3], nrow(path2)))
animate_xy(dt, col = color)
animate_slice(dt, col = color, cex_slice = 1, cex_other = 1)

# 4) sphere,  holes_2d_better and holes_2d_better_max_tries
path1 <- holes_2d_better$basis %>% flatten_dbl() %>% matrix(ncol = 6, byrow = TRUE)
path2 <- holes_2d_better_max_tries$basis %>% flatten_dbl() %>% matrix(ncol = 6, byrow = TRUE)
dt <- rbind(sphere, path1, path2)
colnames(dt) <- c(map_chr(1:6, ~paste0("x", .x)))
pal <- c("#1B9E77","#D95F02","#7570B3", "#E7298A")
color <- c(rep(pal[1], nrow(sphere)),
           rep(pal[2], nrow(path1)),
           rep(pal[3], nrow(path2)))
animate_xy(dt, col = color, cex_slice = 0.8)
animate_slice(dt, col = color)

##### PCA
path1 <- holes_2d_better$basis  %>% flatten_dbl() %>% matrix(ncol = 12, byrow = TRUE)
path2 <- holes_2d_better_max_tries$basis %>% flatten_dbl() %>% matrix(ncol = 12, byrow = TRUE)

dt <- rbind(sphere, path2, path1)
pca <- dt %>% prcomp()
summary(pca)
pca_pred <- predict(pca) %>%
  as_tibble() %>%
  mutate(max_tries = c(rep("sphere", nrow(sphere)),
                       rep(25, nrow(path1)),
                       rep(500, nrow(path2))))
p1 <- pca_pred %>% filter(max_tries != 500) %>%
  ggplot(aes(x = -PC2, y = PC1, color = max_tries)) +
  geom_point() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Dark2")

dt <- rbind(sphere, path2)
pca <- dt %>% prcomp()
summary(pca)
pca_pred <- predict(pca) %>%
  as_tibble() %>%
  mutate(max_tries = c(rep("sphere", nrow(sphere)),
                       rep(500, nrow(path2))))
p2 <- pca_pred %>%
  ggplot(aes(x = -PC2, y = PC1, color = max_tries)) +
  geom_point() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Dark2")


dt <- rbind(sphere, path1)
pca <- dt %>% prcomp()
summary(pca)
p3 <- predict(pca) %>%
  as_tibble() %>%
  mutate(max_tries = c(rep("sphere", nrow(sphere)),
                       rep(25, nrow(path1)))) %>%
  ggplot(aes(x = PC1, y = PC2, color = max_tries)) +
  geom_point() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Dark2" )

(p2 | p3) & theme(legend.position = "bottom")


##### MDS
dt <- rbind(sphere, path2, path1)
d <- dist(dt)
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit$points %>%
  as_tibble() %>%
  mutate(max_tries = c(rep("sphere", nrow(sphere)),
                       rep(500, nrow(path2)),
                       rep(25, nrow(path1)))) %>%
  ggplot(aes(x = V1, y = V2, color = max_tries)) +
  geom_point() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Dark2")
