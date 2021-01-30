origin <- interrupt_no

clean_before <- origin %>%
  select(basis, index_val, info, method, tries, loop, id) %>%
  mutate( id = row_number(),
          loop = ifelse(id == 1, 1, loop),
          method = ifelse(id == 1, "search_better", method),
          tries = ifelse(id == 1, tries, tries + 1)) %>%
  group_by(tries, info) %>%
  mutate(loop = ifelse(is.na(loop), row_number(), loop)) %>%
  ungroup()

last <- clean_before %>% filter(info == "new_basis") %>% mutate(info = "interpolation")

before <- bind_rows(clean_before, last) %>%
  arrange(tries) %>%
  mutate(search = ifelse(info == "interpolation", "interp", "search")) %>%
  group_by(tries, search) %>%
  mutate(loop = row_number()) %>%
  ungroup() %>%
  mutate(id = row_number()) %>%
  dplyr::select(-search)

origin2 <- interrupt_yes

after <- origin2 %>%
  select(basis, index_val, info, method, tries, loop, id) %>%
  mutate( id = row_number(),
          loop = ifelse(id == 1, 1, loop),
          method = ifelse(id == 1, "search_better", method),
          tries = ifelse(id == 1, tries, tries + 1),
          search = ifelse(info == "interpolation", "interp", "search")) %>%
  group_by(tries, search) %>%
  mutate(loop = row_number()) %>%
  ungroup() %>%
  dplyr::select(-search)


save(before, file = here::here("data", "before.rda"))
save(after, file = here::here("data", "after.rda"))
