library(tidyverse)

test <- read_lines("Time:      7  15   30
Distance:  9  40  200")

dat <- read_lines("input/day06.txt")


##PART 1
dat %>% 
  str_extract_all("\\d+") %>% 
  set_names(c("time", "record")) %>% 
  map(parse_number) %>%
  bind_rows() %>% 
  mutate(race = str_c("race", 1:nrow(.)), .before = time) %>% 
  mutate(press = map(time, ~seq(1:.x))) %>% 
  unnest(press) %>% 
  filter(time != press) %>% 
  mutate(duration = time - press) %>% 
  mutate(distance = press*duration)%>% 
  filter(distance > record) %>% 
  count(race) %>% 
  summarise(total = prod(n)) %>% 
  pull

##PART2
dat %>% 
  str_extract_all("\\d+") %>% 
  set_names(c("time", "record")) %>% 
  map(str_flatten) %>% 
  map(parse_number) %>% 
  bind_rows() %>% 
  mutate(race = str_c("race", 1:nrow(.)), .before = time) %>% 
  mutate(press = map(time, ~seq(1:.x))) %>% 
  unnest(press) %>% 
  filter(time != press) %>% 
  mutate(duration = time - press) %>% 
  mutate(distance = press*duration)%>% 
  filter(distance > record) %>% 
  count(race) %>% 
  summarise(total = prod(n)) %>% 
  pull
  
