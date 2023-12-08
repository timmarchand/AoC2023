library(tidyverse)
dat <- read_lines("input/day04.txt")
test <- read_lines("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

## PART 1
test %>% 
  tibble(id = .) %>% 
  separate_wider_delim(cols = id, names = c("id", "win", "card"), delim = regex(":|\\|")) %>% 
  mutate(across(-id, ~str_squish(.x) %>%  str_split(" "))) %>%
  mutate(match = map2_int(win,card, ~.y %in% .x %>% sum),
         score = ifelse(match == 0, 0, 2^(match-1))) %>% 
  summarise(sum = sum(score, na.rm = TRUE)) 


## PART 2
cards <- dat %>% 
  tibble(id = .) %>% 
  separate_wider_delim(cols = id, names = c("id", "win", "card"), delim = regex(":|\\|")) %>% 
  mutate(across(-id, ~str_squish(.x) %>%  str_split(" "))) %>%
  transmute(
          match = map2_int(win,card, ~.y %in% .x %>% sum), count = 1) 


for (i in 1:nrow(cards)) {
  if (cards$match[i] == 0) {next}
    cards$count[i + 1:cards$match[i]] <- (cards$count[i] + cards$count[i + 1:cards$match[i]])
}

summarise(cards, sum = sum(count))

