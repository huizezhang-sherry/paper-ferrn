## ---- load-pkg
library(ferrn)
library(tidyverse)
files <- paste0("data/", list.files(here::here("data")))
purrr::walk(.x = files, ~load(here::here(.x), env = globalenv()))

## ---- toy-search
bind_rows(holes_2d_better_max_tries %>% mutate(max_tries = 500),
          holes_2d_better %>% mutate(max_tries = 25)) %>%
  explore_trace_search(group = max_tries) +
  scale_color_botanical(palette = "daisy") +
  facet_wrap(vars(max_tries),labeller = "label_both", ncol = 2) +
  xlab("iteration")
