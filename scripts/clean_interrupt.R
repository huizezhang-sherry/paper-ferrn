origin <- interrupt_no

before <- origin %>%
  select(basis, index_val, info, method, tries, loop, id) %>%
  mutate( id = row_number(),
          loop = ifelse(id == 1, 1, loop),
          method = ifelse(id == 1, "search_better", method),
          tries = ifelse(id == 1, tries, tries + 1)) %>%
  group_by(tries, info) %>%
  mutate(loop = ifelse(is.na(loop), row_number(), loop)) %>%
  ungroup()

origin2 <- interrupt_yes

after <- origin2 %>%
  select(basis, index_val, info, method, tries, loop, id) %>%
  mutate( id = row_number(),
          loop = ifelse(id == 1, 1, loop),
          method = ifelse(id == 1, "search_better", method),
          tries = ifelse(id == 1, tries, tries + 1)) %>%
  group_by(tries, info) %>%
  mutate(loop = ifelse(is.na(loop), row_number(), loop)) %>%
  ungroup()


save(before, file = here::here("data", "before.rda"))
save(after, file = here::here("data", "after.rda"))
