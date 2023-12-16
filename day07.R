library(tidyverse)

test <- read_lines("32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

levels <- c(1:9,"T","J","Q","K","A")
levels2 <- c("J", 1:9,"T","Q","K","A")

dat <- read_lines("input/day07.txt")

dat %>% 
  tibble(cards = .) %>% 
  separate_wider_delim(cards, delim = " ", names = c("cards", "bid")) %>% 
  separate_wider_regex(cards, patterns = c(c1 = "^.",
                                           c2 = "(?<=.).",
                                           c3 = "(?<=..).",
                                           c4 = "(?<=...).",
                                           c5 = ".$")) %>% 
  mutate(across(starts_with("c"), ~factor(.x, levels = levels))) %>%
  rowid_to_column() %>% 
  pivot_longer(cols = starts_with("c"), names_to = "card", values_to = "value") %>% 
  group_by(rowid) %>% 
   add_count(value) %>% 
   mutate(nn = sum(n), .) %>% 
  ungroup() %>% 
  select(-c(rowid, n)) %>% 
  pivot_wider(names_from = card, values_from = value) %>% 
  arrange(nn,c1,c2,c3,c4,c5) %>% 
  rowid_to_column()  %>% 
  mutate(win = (rowid*as.numeric(bid))) %>% 
  summarise(sum = sum(win)) %>% 
  pull

### part 2
## first deal with hands with no Js
non_J <- dat %>% 
  tibble(cards = .) %>% 
  separate_wider_delim(cards, delim = " ", names = c("cards", "bid")) %>% 
  filter(!str_detect(cards, "J"))%>% 
  separate_wider_regex(cards, patterns = c(c1 = "^.",
                                           c2 = "(?<=.).",
                                           c3 = "(?<=..).",
                                           c4 = "(?<=...).",
                                           c5 = ".$")) %>% 
  mutate(across(starts_with("c"), ~factor(.x, levels = levels))) %>%
  rowid_to_column() %>% 
  pivot_longer(cols = starts_with("c"), names_to = "card", values_to = "value") %>% 
  group_by(rowid) %>% 
  add_count(value) %>% 
  mutate(nn = sum(n), .) %>% 
  ungroup() %>% 
  select(-c(rowid, n)) %>% 
  pivot_wider(names_from = card, values_from = value) %>% 
  arrange(nn,c1,c2,c3,c4,c5)

## now deal with Js
# with_J
with_J <- dat %>% 
  tibble(cards = .) %>% 
  separate_wider_delim(cards, delim = " ", names = c("cards", "bid")) %>% 
  filter(str_detect(cards, "J")) %>% 
  separate_wider_regex(cards, patterns = c(c1 = "^.",
                                           c2 = "(?<=.).",
                                           c3 = "(?<=..).",
                                           c4 = "(?<=...).",
                                           c5 = ".$")) %>% 
  mutate(across(starts_with("c"), ~factor(.x, levels = levels2))) %>%
  rowid_to_column() %>% 
  pivot_longer(cols = starts_with("c"), names_to = "card", values_to = "value") 


with_J %>% 
  group_by(rowid) %>% 
  add_count(value) %>% 
  ## filter most common card
  filter(value != "J") %>% 
  filter(n == max(n)) %>% 
  arrange(desc(value)) %>% 
  ## take highest value most common card
  summarise(replace_with = first(value)) %>%
  left_join(with_J,.) %>% 
  ## replace Js with most common and highest value card
  mutate(value2 = case_when(value != "J" ~ value, 
                            value == "J"& is.na(replace_with) ~ "J",
                            TRUE ~ replace_with)) %>% 
  group_by(rowid) %>% 
  add_count(value2) %>% 
  mutate(nn = sum(n), .) %>% 
  ungroup() %>% 
  select(-c(rowid, n, value2, replace_with)) %>%
  pivot_wider(names_from = card, values_from = c(value)) %>%
  arrange(nn,c1,c2,c3,c4,c5) %>% 
  # # join with non J set
   bind_rows(non_J) %>% 
   arrange(nn,c1,c2,c3,c4,c5) %>% 
  rowid_to_column()  %>%
  mutate(win = (rowid*as.numeric(bid))) %>% 
  summarise(sum = sum(win)) %>% 
  pull
