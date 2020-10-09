## ----data-object
# set.seed(123456)
# holes_1d_better <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
#                                                               search_f =  search_better),
#                                 rescale = FALSE, verbose = TRUE)
# save(holes_1d_better, file = here::here("data/holes_1d_better.rda"))

holes_1d_better %>%
  dplyr::select(id, basis, index_val, info, tries, loop, method, alpha) %>%
  head(5)
