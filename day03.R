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
x
