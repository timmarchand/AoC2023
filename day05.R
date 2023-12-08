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
  map_df(bind_rows)

index <- tibble(index = 0:99, d = 0:99, s = 0:99)

index %>% 
  full_join(maps$seed_to_soil, join_by(d == d, s == s)) %>% 
              select(-index))

maps %>% 
  map(~summarise(.x, across(everything(),
                 min = min,
                 max = max)))

maps$seed_to_soil %>% 
 mutate(max_d = max(s))

expand_range <- function(tbl, col1, col2){
 
   tbl %>% 
    expand({{col1}} = 0:max{{col1}},
           {{col2}} = 0:max{{col2}})
}
  
}

                       