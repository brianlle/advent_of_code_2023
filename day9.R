# day 9

library(dplyr)
library(stringr)

data <- read.csv("input/day9.txt", sep = " ", header = FALSE)

get_diffs <- function(array){
  diffs <- rep(0, length(array)-1)
  for (idx in 1:(length(array)-1)){
    diffs[idx] <- array[idx+1] - array[idx]
  }
  return(diffs)
}

find_next_value <- function(array){
  last_value <- array[length(array)]
  cur_add <- 0
  #layers <- 0
  while (length(unique(array)) != 1){
    array <- get_diffs(array)
    cur_add <- cur_add + array[length(array)]
    #layers <- layers + 1
  }
  
  return(last_value + cur_add)
}

total_pred <- 0

for (idx in 1:nrow(data)){
  total_pred <- total_pred + find_next_value(data[idx,] |> as.numeric())
}

print(total_pred)

# part 2

find_prev_value <- function(array){
  last_value <- array[1]
  cur_add <- 0
  cur_sign <- -1
  while (length(unique(array)) != 1){
    array <- get_diffs(array)
    cur_add <- cur_add + array[1]*cur_sign
    cur_sign <- cur_sign*-1
  }
  
  return(last_value + cur_add)
}

total_pred <- 0

for (idx in 1:nrow(data)){
  total_pred <- total_pred + find_prev_value(data[idx,] |> as.numeric())
}

print(total_pred)

# 114953693 - no