library(tidyverse)

dat <- read_lines("input/day02.txt")

test <- read_lines("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")


ball_max <- dat %>% 
  tibble(id = .) %>% 
  separate_wider_delim(cols = id, delim = ":", names = c("id", "games")) %>% 
  separate_longer_delim(games, ";") %>% 
  separate_longer_delim(games, ",") %>% 
  mutate(id = parse_number (id),
         games = str_squish(games)) %>% 
  separate_wider_delim(cols = games, delim = " ", names = c("n", "colour")) %>% 
  mutate(n = parse_number(n)) %>% 
   summarise(max = max(n), .by = c(id, colour)) %>% 
  pivot_wider(names_from = colour, values_from = max) 

ball_max %>% 
filter(blue <= 14 , red <= 12 , green <=13) %>% 
  summarise(sum(id))
  
ball_max %>% 
  summarise(product = blue * red * green, .by = id) %>% 
  summarise(sum(product))

