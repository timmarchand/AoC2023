library(tidyverse)
dat <- read_lines("input/day09.txt") %>% 
  str_extract_all("[-\\d]+") %>% 
  map(parse_integer)



find_next <- function(vec){
result <- list()


result[[1]] <- {{vec}}

while(sum(result[[length(result)]])){
i <- length(result) + 1
result[[i]] <- diff(result[[i-1]])
}
result %>% 
  map_int(last) %>% 
  sum
}



find_first <- function(vec){
result <-  list()
result[[1]] <- {{vec}}

while(sum(result[[length(result)]])){
i <- length(result) + 1
result[[i]] <- diff(result[[i-1]])
}

vec2 <- result %>% 
  map_int(first) %>% 
  unlist

 res <-  vector("list", length = length(vec2) -1)
   res[[length(vec2)]] <- 0
   end <- length(vec2) - 1
 for(j in end:1){
   res[[j]] <- vec2[j] - res[[j+1]]
 }
return(res[[1]])


## part 1
dat %>% 
  map(find_next) %>% 
  unlist %>% 
  sum

## part 2
dat %>% 
  map(find_first) %>% 
  unlist() %>% 
  sum
