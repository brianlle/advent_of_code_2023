# day 14
library(stringr)
library(dplyr)

data <- readLines("input/day14.txt")
#data <- readLines("input/test.txt")

row_length <- length(data)
data <- data |> str_split(pattern = "") |> unlist() |> matrix(nrow = row_length) |> t()

data[data == "O"] <- 1
data[data == "."] <- 0
data[data == "#"] <- 2

data <- matrix(as.numeric(data), nrow = row_length)

can_move <- function(rock1, rock2){ # can it move from position rock2 to position rock1?
  if (rock2 == 1 & rock1 == 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

move_rock_north <- function(matrix){
  
  # north: rocks roll upwards, so in the negative [-idx,] direction
  
  for (column in 1:ncol(matrix)){ # iterate individual columns
    # repeat nrow times
    cur_row <- nrow(matrix)
    while (cur_row > 1){
      for (row in 2:cur_row){
        if (can_move(matrix[row-1, column], matrix[row, column])){
          matrix[row-1, column] <- 1
          matrix[row, column] <- 0
        }
      }
      cur_row <- cur_row - 1
    }
  }
  return(matrix)
}

moved_data <- move_rock_north(data)

load <- 0
for (row in 1:nrow(moved_data)){
  num_rocks <- table(moved_data[row,])["1"] |> as.numeric()
  if (is.na(num_rocks)){
    num_rocks <- 0
  }
  load <- load + num_rocks*(nrow(moved_data)+1-row)
}
load

# part 2
# roll N, then W, then S, then E; repeat 1000000000 CYCLES (4 spins), then calculate load
# rotate clockwise then roll N is same as rolling W, etc
rotate <- function(x) t(apply(x, 2, rev))


# cycle and store positions until we see one we've seen before?
keep_searching <- TRUE
cur_step <- 1
cur_data <- data
searched <- c()
while (keep_searching){
  print(cur_step)
  for (rep in 1:4){
    cur_data <- move_rock_north(cur_data)
    cur_data <- rotate(cur_data)
  }
  
  cur_string <- cur_data[1:(nrow(cur_data)^2)] |> paste0(collapse = "")
  if (cur_string %in% names(searched)){
    keep_searching <- FALSE
  } else {
    searched[cur_data[1:(nrow(cur_data)^2)] |> paste0(collapse = "")] <- 1
    cur_step <- cur_step + 1
    cur_direction <- (cur_direction + 1) %% 4
  }
}

# we're now in a CYCLE, need to figure out how long the cycle is and where we will be
last_cycle <- which(names(searched) == cur_data[1:(nrow(cur_data)^2)] |> paste0(collapse = ""))
cycle_time <- cur_step - last_cycle

# finish the remaining cycles
reps_remaining <- (1000000000 - cur_step) %% cycle_time
for (idx in 1:reps_remaining){
  for (rep in 1:4){
    cur_data <- move_rock_north(cur_data)
    cur_data <- rotate(cur_data)
  }
}

load <- 0
for (row in 1:nrow(cur_data)){
  num_rocks <- table(cur_data[row,])["1"] |> as.numeric()
  if (is.na(num_rocks)){
    num_rocks <- 0
  }
  load <- load + num_rocks*(nrow(cur_data)+1-row)
}
print(load)
