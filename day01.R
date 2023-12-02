library(tidyverse)

## PART 1
dat <- read_lines("input/day01.txt")


dat %>% 
  str_extract_all("\\d") %>% 
  map_chr(~str_c(first(.x), last(.x))) %>% 
  parse_number %>% 
  sum



## PART 2
replacements <- tibble( old = c("one","two","three","four","five","six","seven","eight","nine"),
                        new = c("o1e","t2o","th3ee","f4r","f5ve","s6x","se7en","ei8ht","ni9ne")) %>% 
  deframe

dat %>% 
  str_replace_all(replacements) %>% 
  str_extract_all("\\d") %>% 
  map_chr(~str_c(first(.x), last(.x))) %>% 
  parse_number %>% 
  sum
