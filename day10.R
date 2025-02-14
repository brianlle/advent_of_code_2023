# day 10
library(stringr)
library(dplyr)

data <- read.csv("input/day10.txt", sep = "", header = FALSE)
#data <- read.csv("input/test.txt", sep = "", header = FALSE)

# S at row 93, col 44 (y = 93, x = 44)

# x_df <- matrix(nrow = nrow(data), ncol = nchar(data[1,]))
# y_df <- matrix(nrow = nrow(data), ncol = nchar(data[1,]))
# 
# get_x <- function(value){
#   if (value %in% c("-", "L", "F")){
#     x <- 1 # +x is east, -x is west
#   } else if (value %in% c(""))
# }


# flip data on y-axis so we can use north = positive

new_data <- data.frame()
for (idy in 1:nrow(data)){
  cur_row <- data[idy,] |> str_split_1(pattern = "")
  # for (idx in 1:length(cur_row)){
  #   x_df[idy,idx] <- get_x(cor_row[idx])
  #   y_df[idy,idx] <- get_y(cor_row[idx])
  # }
  new_data <- rbind(cur_row, new_data) # flip data on y-axis so we can use north = positive
}
colnames(new_data) <- paste0("R", 1:ncol(new_data))

# S at row 93, col 44 (y = 93, x = 44)
# visual inspection shows that S is an h-bar, so let's just go right
#coords <- c(x_cur = 93, y_cur = 44, x_prev = 92, y_prev = 44)


move <- function(coords, movement){
  
  new_coords <- coords
  new_coords["x_cur"] <- coords["x_cur"]
  new_coords["y_cur"] <- coords["y_cur"]
  new_coords["x_prev"] <- coords["x_cur"]
  new_coords["y_prev"] <- coords["y_cur"]
  
  if (movement == "|"){
    if (coords["y_cur"] > coords["y_prev"]){
      new_coords["y_cur"] <- coords["y_cur"] + 1
    } else {
      new_coords["y_cur"] <- coords["y_cur"] - 1
    }
  } else if (movement == "-"){
    if (coords["x_cur"] > coords["x_prev"]){
      new_coords["x_cur"] <- coords["x_cur"] + 1
    } else {
      new_coords["x_cur"] <- coords["x_cur"] - 1
    }
  } else if (movement == "L"){
    if (coords["x_cur"] < coords["x_prev"]){ # moved left previously, moving up now
      new_coords["y_cur"] <- coords["y_cur"] + 1
    } else { # moved down before, now moving right
      new_coords["x_cur"] <- coords["x_cur"] + 1
    }
  } else if (movement == "J"){
    if (coords["x_cur"] > coords["x_prev"]){ # moved right previously, moving up now
      new_coords["y_cur"] <- coords["y_cur"] + 1
    } else { # moved up before, now moving left
      new_coords["x_cur"] <- coords["x_cur"] - 1
    }
  } else if (movement == "7"){
    if (coords["x_cur"] > coords["x_prev"]){ # moved right previously, moving down now
      new_coords["y_cur"] <- coords["y_cur"] - 1
    } else { # moved up before, now moving left
      new_coords["x_cur"] <- coords["x_cur"] - 1
    }
  } else if (movement == "F"){
    if (coords["x_cur"] < coords["x_prev"]){ # moved left previously, moving down now
      new_coords["y_cur"] <- coords["y_cur"] - 1
    } else { # moved up before, now moving right
      new_coords["x_cur"] <- coords["x_cur"] + 1
    }
  }
  
  return(new_coords)
}

# S at row 93, col 44 (y = 93, x = 44)
# visual inspection shows that S is an h-bar, so let's just go right


# for testing:
# coords <- c(x_cur = 2, y_cur = 4, x_prev = 3, y_prev = 4)
# next_move <- "F"

coords <- c(x_cur = 44, y_cur = 48, x_prev = 43, y_prev = 48)
moves <- 0
next_move = "-"
while (next_move != "S"){
  coords <- move(coords, next_move)
  moves <- moves + 1
  next_move <- new_data[coords["y_cur"], coords["x_cur"]]
  if (next_move == "."){
    print(coords)
    break
  }
}
moves # this should be even...
# not 1699 [(3397+1)/2], also not 1698

# | is a vertical pipe connecting north and south.
# - is a horizontal pipe connecting east and west.
# L is a 90-degree bend connecting north and east.
# J is a 90-degree bend connecting north and west.
# 7 is a 90-degree bend connecting south and west.
# F is a 90-degree bend connecting south and east.
# . is ground; there is no pipe in this tile.
# S is the starting position of the animal; 
# there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

# pipe is a loop
# get length of loop, divide by 2

# convert input to matrices for x, y movement

# part 2
# need to calculate number of tiles inside the loop (not including loop itself)
# step 1, create a matrix of the loop as 0s / 1s?
loop <- matrix(0, nrow = 140, ncol = 140)

coords <- c(x_cur = 44, y_cur = 48, x_prev = 43, y_prev = 48)
moves <- 0
loop[coords["y_cur"], coords["x_cur"]] <- 1
next_move = "-"
while (next_move != "S"){
  coords <- move(coords, next_move)
  moves <- moves + 1
  loop[coords["y_cur"], coords["x_cur"]] <- 1 #(moves %% 8) + 1
  next_move <- new_data[coords["y_cur"], coords["x_cur"]]
  if (next_move == "."){
    print(coords)
    break
  }
  print(moves)
}

# now to calculate the area of the loop....
write.table(loop, "yolo.txt", sep = "", col.names = FALSE, row.names = FALSE)

# after visually inspecting, we can just go through and search all the neighbor tiles,
# and we can truncate edge to avoid programming edge cases

# find vertical bars, go through area, add if not part of loop

area <- 0
for (idy in 1:nrow(loop)){
  cur_row <- which(new_data[idy,] %in% "|")
  # check if pipe is part of loop
  cur_row <- cur_row[loop[idy, cur_row] == 1]
  if (length(cur_row) > 1){
    if ((length(cur_row) %% 2) == 0){ # even row
      for (iter in 1:(length(cur_row)/2)){
        for (idx in cur_row[2*iter-1]:cur_row[2*iter]){
          if (loop[idy, idx] != 1){
            area <- area + 1
          }
        }
        #area <- area + cur_row[2*iter] - cur_row[2*iter-1] - 1
      }
    } else { # odd row
      # cur_row <- cur_row[-mean(1:length(cur_row))]
      # for (iter in 1:(length(cur_row)/2)){

      for (iter in 1:((length(cur_row)-1)/2)){
        for (idx in cur_row[2*iter-1]:cur_row[2*iter]){
          if (loop[idy, idx] != 1){
            area <- area + 1
          }
        }
        #area <- area + cur_row[2*iter] - cur_row[2*iter-1] - 1
      }
    }
  }
}

area

# not 401, tho that's the right answer for someone else lul
# not 413, tho that's the right answer for someone else lul
# 514 is too high