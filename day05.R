library(tidyverse)
#test data
{test <- "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"
}
# input data
dat <- read_lines("input/day05.txt")

seeds <- read_lines(test, n_max = 1) %>% 
  str_split_1("\\D") %>% 
  parse_number() %>% 
  .[!is.na(.)]

maps %>% 
  map_df(bind_rows) %>% 
  summarise(min_s = min(s),
            min_d = min(d),
            max_s = max(s),
            max_d = max(d)) %>% 
  select(min_s, max_s) %>% glimpse
  deframe %>% str
  seq(.[1]:.[2])
  bind_rows()

maps <- read_lines(test, skip = 2) %>% 
  tibble(maps = .) %>% 
  filter(maps != "") %>% 
  mutate(type = str_detect(maps, ":")) %>% 
  mutate(id = cumsum(type)) %>% 
  mutate(map = str_flatten(maps, " ") %>% 
           str_extract("^\\w+-to-\\w+"), .by = id) %>% 
  filter(!type) %>% 
  select(map, range = maps) %>% 
  separate_wider_delim(cols = range, names = c("dest", "source", "length"), delim = " ") %>%
  mutate(across(dest:length, parse_number)) %>%  
  mutate(d = map2(dest, length, ~seq(from = .x, length.out = .y)),
         s = map2(source, length, ~seq(from = .x, length.out = .y))) %>% 
  select(map, d,s) %>% 
  unnest(cols = c(d,s)) 

maps$fertilizer_to_water %>% 
  anti_join(index,., join_by("s")) %>% 
  bind_rows(maps$fertilizer_to_water,.) %>% arrange(s)
  fill(map, .direction = "downup")

nms <- maps %>% 
  distinct(map) %>% 
  arrange(map) %>% 
  pull

maps <- maps %>% 
  group_split(map) %>% 
  set_names(janitor::make_clean_names(nms))

maps %>% 
  map(~full_join(index,.x, join_by(d == d, s == s)) %>% 
        fill(map, .direction = "downup") %>% 
        select(-index)) %>% 
  map_df(bind_rows) %>% 
  group_split(map) %>% 
  set_names(janitor::make_clean_names(nms))

index <- tibble(d = 0:99, s = 0:99)

index %>% 
  full_join(maps$seed_to_soil, join_by(d == d, s == s)) %>% 
              select(-index)%>% 
  fill(map, .direction = "downup") %>% 
  arrange(s)

maps %>% 
  map(~full_join(.x, index, join_by(d == d, s == s))) %>% pluck("soil_to_fertilizer") %>% print(n = 100)
  # pluck("seed_to_soil") %>% 
  # filter(s %in% seeds) %>% 
  # select(s = d) %>% 
  inner_join(pluck(maps, "soil_to_fertilizer"),.) %>%  
  select(s = d) %>% 
  inner_join(pluck(maps, "fertilizer_to_water"),.) %>% 
  select(s = d) %>% 
  inner_join(pluck(maps, "water_to_light"),.) %>% 
  select(s = d) %>%   
  inner_join(pluck(maps, "light_to_temperature"),.) %>% 
  select(s = d) %>% 
  inner_join(pluck(maps, "temperature_to_humidity"),.)# %>% 
  select(s = d) %>% 
  inner_join(pluck(maps, "humidity_to_location"),.)
  
  
  
  

seeds <- seeds %>% 
  str_extract_all("\\d+") %>% 
  unlist() %>% 
  parse_number()
  
  maps %>%
  pluck("seed_to_soil") %>% 
  filter(s %in% seeds) %>% 
    select(s = d) %>% 
    inner_join(pluck(maps, "soil_to_fertilizer"),.)


maps$soil_to_fertilizer %>% 
  arrange(s)

maps$seed_to_soil %>% 
 mutate(max_d = max(s))

expand_range <- function(tbl, col1, col2){
 
   tbl %>% 
    expand({{col1}} = 0:max{{col1}},
           {{col2}} = 0:max{{col2}})
}
  
}

                       