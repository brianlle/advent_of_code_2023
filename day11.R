# day 11
library(stringr)
library(dplyr)

data <- readLines("input/day11.txt")
#data <- readLines("input/test.txt")


new_data <- matrix(nrow = length(data), ncol = length(data))

for (idx in 1:length(data)){
  new_data[idx,] <- str_split_1(data[idx], pattern = "")
}

# start by expanding space, find rows/cols that are all empty
row_exp <- c()
for (row in 1:nrow(new_data)){
  if (all(new_data[row,] == ".")){
    row_exp <- c(row_exp, row)
  }
}
col_exp <- c()
for (col in 1:nrow(new_data)){
  if (all(new_data[,col] == ".")){
    col_exp <- c(col_exp, col)
  }
}

# expand row first
new_data_2 <- matrix(nrow = length(data) + length(row_exp), ncol = length(data))

cur_row <- 1
for (idx in 1:nrow(new_data)){
  if (idx %in% row_exp){
    new_data_2[cur_row,] <- new_data[idx,]
    cur_row <- cur_row + 1
    new_data_2[cur_row,] <- new_data[idx,]
  } else {
    new_data_2[cur_row,] <- new_data[idx,]
  }
  cur_row <- cur_row + 1
}

# expand col
new_data_3 <- matrix(nrow = length(data) + length(row_exp), ncol = length(data) + length(col_exp))

cur_col <- 1
for (idx in 1:ncol(new_data_2)){
  if (idx %in% col_exp){
    new_data_3[,cur_col] <- new_data_2[,idx]
    cur_col <- cur_col + 1
    new_data_3[,cur_col] <- new_data_2[,idx]
  } else {
    new_data_3[,cur_col] <- new_data_2[,idx]
  }
  cur_col <- cur_col + 1
}

# find #, store position
x_pos <- c()
y_pos <- c()
for (idx in 1:nrow(new_data_3)){
  for (idy in 1:ncol(new_data_3)){
    if (new_data_3[idx, idy] == "#"){
      x_pos <- c(x_pos, idx)
      y_pos <- c(y_pos, idy)
    }
  }
}

total_dist <- 0
for (idx in 1:(length(x_pos)-1)){
  for (idx2 in (idx+1):length(x_pos)){
    if (idx == idx2){
      # nothing
    } else if (idx2 > idx){
      total_dist <- total_dist + abs(x_pos[idx] - x_pos[idx2]) + abs(y_pos[idx] - y_pos[idx2])
    }
  }
}

total_dist

# part 2
# instead of doubling, an empty row or empty column becomes 1 million
# store position of original, non-expanded positions
# find #, store position
x_pos <- c()
y_pos <- c()
for (idx in 1:nrow(new_data)){
  for (idy in 1:ncol(new_data)){
    if (new_data[idx, idy] == "#"){
      x_pos <- c(x_pos, idx)
      y_pos <- c(y_pos, idy)
    }
  }
}

# still have row_exp and col_exp numbers
row_exp
col_exp

# test caes: factor = 10, actual: factor = 1000000
calculate_distance <- function(x1, y1, x2, y2, row_exp, col_exp, factor){
  rows <- 0
  cols <- 0
  for (row in row_exp){
    if ((row > x1 & row < x2) | (row < x1 & row > x2)){
      #distance <- distance + (factor-1)
      rows <- rows + 1
    }
  }
  for (col in col_exp){
    if ((col > y1 & col < y2) | (col < y1 & col > y2)){
      #distance <- distance + (factor-1)
      cols <- cols + 1
    }
  }
  
  distance <- abs(x1 - x2) + abs(y1 - y2) + (rows + cols)*(factor - 1)
  
  return(distance)
}

total_dist <- 0
for (idx in 1:(length(x_pos)-1)){
  for (idx2 in (idx+1):length(x_pos)){
    if (idx == idx2){
      # nothing
    } else if (idx2 > idx){
      total_dist <- total_dist + 
        calculate_distance(x_pos[idx], y_pos[idx], x_pos[idx2],y_pos[idx2], row_exp, col_exp, factor = 1000000)
    }
  }
}
total_dist
