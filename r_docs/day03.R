library(tidyverse)

dat <- read_lines("input/day03.txt")

test <- read_lines("467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

str_locate_all(test, "\\d+")

  test %>%
    enframe(name = "id", value = "text") %>%
    mutate(positions = map(text, ~str_locate_all(.x, "\\d+"))) %>% 
    unnest(positions) %>% 
  unnest(positions) %>% 
    transmute(id, text, 
              start = positions[,1],
              end = positions[,2])

width = nchar(test[1])

## create a long character vector
 x <- dat %>% 
  str_split("") %>% 
   unlist %>% 
   str_replace("[^\\d\\.]", "@") %>% 
   matrix(ncol = width, byrow =TRUE)
 



## function to combine adjacent numbers forwards
conc_fun1 <- function(vec){
  for(i in 2:length(vec)){
    if(vec[i] == "."){next} else
    vec[i] <- str_c(vec[i-1],vec[i])
  }
  vec
}

## function to combine adjacent numbers backwards
conc_fun2 <- function(vec){
  for(i in (length(vec) - 1):1){
     if(str_detect(vec[i], "\\d") &
      str_detect(vec[i+1], "\\d")){vec[i] <- vec[i+1]} 
}
  str_remove_all(vec,"(\\.|@)")
  
  }

## put functions together
conc_fun <- function(vec){ conc_fun1(vec) %>% conc_fun2}


## combine adjacent numbers
for(row in 1:nrow(x)){
  x[row,] <- conc_fun(x[row,])
}
x

## set negative anchor grid
## add border rows and columns to x
y <- cbind(rep(".", nrow(x)), x, rep(".", nrow(x))) %>% 
      rbind(rep(".", ncol(.)),.,rep(".", ncol(.)))

z <- TRUE
## if adjacent to anchor, set to FALSE
z <- (matrix(z, nrow = nrow(y), ncol=ncol(y)))



 for(i in 2:nrow(x)){
   for(j in 2:ncol(x)){
     if(y[i-1,j-1] == "@") {z[i,j] <- FALSE} else
        if(y[i,j-1] == "@") {z[i,j] <- FALSE} else
           if(y[i+1,j-1] == "@") {z[i,j] <- FALSE} else
             if(y[i-1,j] == "@") {z[i,j] <- FALSE} else
               if(y[i+1,j] == "@") {z[i,j] <- FALSE} else
                 if(y[i-1,j+1] == "@") {z[i,j] <- FALSE} else
                    if(y[i,j+1] == "@") {z[i,j] <- FALSE} else
                       if(y[i+1,j+1] == "@") {z[i,j] <- FALSE} 
   }
 }

## non anchor grid in x dimensions
x1 <- (z[2:1961, 2:11])

# remove values not adjacent to anchors
x[x1] <- ""

# remove duplicates from the rows
for(row in 1:nrow(x)){
for(col in 2:ncol(x)){
  if(x[row,col-1] == x[row,col]){x[row,col] <- ""} 
}
}

## reddit ----

dat <- read_lines("input/day03Rob.txt")

get_pos_base <- function(pat) {
  p <- str_locate_all(dat, pat)
  t(do.call(rbind, lapply(seq_along(dat), \(k) if (nrow(p[[k]]) > 0) cbind(k, p[[k]]))))
}

get_pos_base("\\d+") %>% tibble() #digit positions
get_pos(dat,"\\d+")
sp <- get_pos("[^0-9\\.]")[1:2, ] #symbol positions

dig <- as.integer(unlist(regmatches(dat, gregexpr("\\d+", dat)))) #digits
sym <- unlist(regmatches(dat, gregexpr("[^0-9\\.]", dat))) #symbols

res <- c(part1 = 0L, part2 = 0L)
for (k in seq_along(sp[1,])) {
  idx <- rowSums(sapply(2:3, \(i) colSums((dp[-i, ] - sp[, k])^2L)) < 4L) > 0L
  res[1]<- res[1] + sum(dig[idx])
  if (sum(idx) == 2L & sym[k] == "*") res[2] <- res[2] + prod(dig[idx])
}

#part1 and 2-----
res


library(tidyverse)

# Assuming 'dat' is a character vector
dat <- c("some example strings", "with numbers 123 and symbols *#+")

# Function to get positions using tidyverse
get_pos <- function(dat, pattern) {
  dat %>%
   enframe(name = "id", value = "text") %>%
    mutate(positions = map(text, ~str_locate_all(.x, "\\d+"))) %>% 
    unnest(positions) %>% 
  unnest(positions) %>% 
    transmute(id, text, 
              start = positions[,1],
              end = positions[,2])
}

# Get positions of digits and symbols
get_pos_base(dat, "\\d+")
sp <- get_pos(dat, "[^0-9\\.]") %>% select(-end) # Select only start positions

# Extract digits and symbols
dig <- str_extract_all(dat, "\\d+") %>% unlist() %>% as.integer()
sym <- str_extract_all(dat, "[^0-9\\.]") %>% unlist()

# Initialize results
res <- tibble(part1 = 0L, part2 = 0L)

# Processing
for (k in seq_along(sp$id)) {
  idx <- map2_dbl(dp$start, dp$id, ~sum((.x - sp$start[k])^2) < 4 & .y == sp$id[k])
  res$part1 <- res$part1 + sum(dig[idx])
  if (sum(idx) == 2 & sym[k] == "*") {
    res$part2 <- res$part2 + prod(dig[idx])
  }
}

res

