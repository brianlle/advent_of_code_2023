# day 16
library(stringr)
library(dplyr)

data <- readLines("input/day16.txt")
#data <- readLines("input/test.txt")

row_length <- length(data)
data <- data |> str_split(pattern = "") |> unlist() |> matrix(nrow = row_length) |> t()

#beams <- list()

# beam starts in top left, moving rightwards
#beams[[1]] <- rep(1, length(data))


update_beam <- function(beam_df, beam_c, data){
  next_x <- beam_c[1]
  next_y <- beam_c[2]
  dir_x <- beam_c[3]
  dir_y <- beam_c[4]
  # check if next_x or next_y is oob
  if ((next_x > ncol(data)) |(next_x < 1) | (next_y > nrow(data)) | (next_y < 1)){
    return(beam_df[-1,])
  } else {
    beam_df[1,] <- beam_c
    return(beam_df)
  }
}

add_beam <- function(beam_df, new_beam, data){
  next_x <- new_beam[1]
  next_y <- new_beam[2]
  dir_x <- new_beam[3]
  dir_y <- new_beam[4]
  if ((next_x > ncol(data)) |(next_x < 1) | (next_y > nrow(data)) | (next_y < 1)){
    return(beam_df)
  } else {
    return(unique(rbind(beam_df, new_beam)))
  }
}

beams_remaining <- data.frame(next_x = 1, next_y = 1, dir_x = 1, dir_y = 0)
prev_beam <- c()
#beams_remaining[[1]] <- c(1,1,1,0) #next_x, next_y, dir_x, dir_y
visited <- matrix(0, nrow = row_length, ncol = row_length)

while (nrow(beams_remaining) > 0){
  next_x <- beams_remaining[1,1]
  next_y <- beams_remaining[1,2]
  dir_x <- beams_remaining[1,3]
  dir_y <- beams_remaining[1,4]
  
  beam_string <- paste(next_x, next_y, dir_x, dir_y, collapse = ",")
  if (beam_string %in% prev_beam){
    beams_remaining <- beams_remaining[-1,]
  }
  
  else {
    prev_beam <- c(prev_beam, beam_string)
    visited[next_y, next_x] <- 1
    
    next_symbol <- data[next_y, next_x]
    if (data[next_y, next_x] == "."){
      next_x <- next_x + dir_x
      next_y <- next_y + dir_y
      beams_remaining <- update_beam(beams_remaining, c(next_x, next_y, dir_x, dir_y), data)
    } else if (data[next_y, next_x] == "|"){
      if (dir_x != 0){
        beams_remaining <- update_beam(beams_remaining, c(next_x, next_y + 1, 0, 1), data)
        beams_remaining <- add_beam(beams_remaining, c(next_x, next_y - 1, 0, -1), data)
      } else if (dir_y != 0){
        next_x <- next_x + dir_x
        next_y <- next_y + dir_y
        beams_remaining <- update_beam(beams_remaining, c(next_x, next_y, dir_x, dir_y), data)
      }
    } else if (data[next_y, next_x] == "-"){
      if (dir_y != 0){
        beams_remaining <- update_beam(beams_remaining, c(next_x+1, next_y, 1, 0), data)
        beams_remaining <- add_beam(beams_remaining, c(next_x-1, next_y, -1, 0), data)
      } else if (dir_x != 0){
        next_x <- next_x + dir_x
        next_y <- next_y + dir_y
        beams_remaining <- update_beam(beams_remaining, c(next_x, next_y, dir_x, dir_y), data)
      }
    } else if (data[next_y, next_x] == "/"){
      if (dir_x > 0){ # going right --> going up
        beams_remaining <- update_beam(beams_remaining, c(next_x, next_y-1, 0, -1), data)
      } else if (dir_x < 0){ # left --> down
        beams_remaining <- update_beam(beams_remaining, c(next_x, next_y+1, 0, 1), data)
      } else if (dir_y > 0){ # down --> left
        beams_remaining <- update_beam(beams_remaining, c(next_x-1, next_y, -1, 0), data)
      } else if (dir_y < 0){ # up --> right
        beams_remaining <- update_beam(beams_remaining, c(next_x+1, next_y, 1, 0), data)
      }
    } else if (data[next_y, next_x] == "\\"){
      if (dir_x > 0){ # going right --> going down
        beams_remaining <- update_beam(beams_remaining, c(next_x, next_y+1, 0, 1), data)
      } else if (dir_x < 0){ # left --> up
        beams_remaining <- update_beam(beams_remaining, c(next_x, next_y-1, 0, -1), data)
      } else if (dir_y > 0){ # down --> right
        beams_remaining <- update_beam(beams_remaining, c(next_x+1, next_y, 1, 0), data)
      } else if (dir_y < 0){ # up --> left
        beams_remaining <- update_beam(beams_remaining, c(next_x-1, next_y, -1, 0), data)
      }
    }
    
   # print(nrow(beams_remaining))
  print(sum(visited))
  }

}


# part 2
# 440 configs
# let's keep track of two things:
# 1. the final beam paths for each initial start
# 2. a record of every beam path + direction visited
  configs <- data.frame()
  configs <- rbind(configs,
                   data.frame(next_x = 1, next_y = 1:nrow(data), dir_x = 1, dir_y = 0))
  configs <- rbind(configs,
                   data.frame(next_x = ncol(data), next_y = 1:nrow(data), dir_x = -1, dir_y = 0))
  configs <- rbind(configs,
                   data.frame(next_x = 1:ncol(data), next_y = 1, dir_x = 0, dir_y = 1))
  configs <- rbind(configs,
                   data.frame(next_x = 1:ncol(data), next_y = nrow(data), dir_x = 0, dir_y = -1))
  
  beam_paths <- list()
  beams_visited_all <- c()
  
  beam_maxes <- c()
  for (idx in 1:nrow(configs)){
    print(idx)
    beams_remaining <- configs[idx,]#data.frame(next_x = 1, next_y = 1, dir_x = 1, dir_y = 0)
    prev_beam <- c()
    #beams_remaining[[1]] <- c(1,1,1,0) #next_x, next_y, dir_x, dir_y
    visited <- matrix(0, nrow = row_length, ncol = row_length)
    
    while (nrow(beams_remaining) > 0){
      next_x <- beams_remaining[1,1]
      next_y <- beams_remaining[1,2]
      dir_x <- beams_remaining[1,3]
      dir_y <- beams_remaining[1,4]
      
      #beam_string <- paste(next_x, next_y, dir_x, dir_y, collapse = ",")
      beam_string <- next_x + next_y*1000 + dir_x*1000000 + dir_y*10000000
      
      # if (beam_string %in% names(beams_visited_all)){
      #   if (beams_visited_all[beam_string] == idx){
      #     cur_idx <- idx
      #     names(cur_idx) <- beam_string
      #     beams_visited_all <- c(beams_visited_all, cur_idx)
      #   } else {
      #     visited <- visited + beam_paths[[beams_visited_all[beam_string]]]
      #     break
      #   }
      # } else {
      #   cur_idx <- idx
      #   names(cur_idx) <- beam_string
      #   beams_visited_all <- c(beams_visited_all, cur_idx)
      # }
      
      if (beam_string %in% prev_beam){
        beams_remaining <- beams_remaining[-1,]
      } else {
        prev_beam <- c(prev_beam, beam_string)
        visited[next_y, next_x] <- 1
        
        next_symbol <- data[next_y, next_x]
        if (data[next_y, next_x] == "."){
          next_x <- next_x + dir_x
          next_y <- next_y + dir_y
          beams_remaining <- update_beam(beams_remaining, c(next_x, next_y, dir_x, dir_y), data)
        } else if (data[next_y, next_x] == "|"){
          if (dir_x != 0){
            beams_remaining <- update_beam(beams_remaining, c(next_x, next_y + 1, 0, 1), data)
            beams_remaining <- add_beam(beams_remaining, c(next_x, next_y - 1, 0, -1), data)
          } else if (dir_y != 0){
            next_x <- next_x + dir_x
            next_y <- next_y + dir_y
            beams_remaining <- update_beam(beams_remaining, c(next_x, next_y, dir_x, dir_y), data)
          }
        } else if (data[next_y, next_x] == "-"){
          if (dir_y != 0){
            beams_remaining <- update_beam(beams_remaining, c(next_x+1, next_y, 1, 0), data)
            beams_remaining <- add_beam(beams_remaining, c(next_x-1, next_y, -1, 0), data)
          } else if (dir_x != 0){
            next_x <- next_x + dir_x
            next_y <- next_y + dir_y
            beams_remaining <- update_beam(beams_remaining, c(next_x, next_y, dir_x, dir_y), data)
          }
        } else if (data[next_y, next_x] == "/"){
          if (dir_x > 0){ # going right --> going up
            beams_remaining <- update_beam(beams_remaining, c(next_x, next_y-1, 0, -1), data)
          } else if (dir_x < 0){ # left --> down
            beams_remaining <- update_beam(beams_remaining, c(next_x, next_y+1, 0, 1), data)
          } else if (dir_y > 0){ # down --> left
            beams_remaining <- update_beam(beams_remaining, c(next_x-1, next_y, -1, 0), data)
          } else if (dir_y < 0){ # up --> right
            beams_remaining <- update_beam(beams_remaining, c(next_x+1, next_y, 1, 0), data)
          }
        } else if (data[next_y, next_x] == "\\"){
          if (dir_x > 0){ # going right --> going down
            beams_remaining <- update_beam(beams_remaining, c(next_x, next_y+1, 0, 1), data)
          } else if (dir_x < 0){ # left --> up
            beams_remaining <- update_beam(beams_remaining, c(next_x, next_y-1, 0, -1), data)
          } else if (dir_y > 0){ # down --> right
            beams_remaining <- update_beam(beams_remaining, c(next_x+1, next_y, 1, 0), data)
          } else if (dir_y < 0){ # up --> left
            beams_remaining <- update_beam(beams_remaining, c(next_x-1, next_y, -1, 0), data)
          }
        }
        
        # print(nrow(beams_remaining))
        #print(paste(idx, sum(visited)))
      }
    }
    print(sum(visited))
    beam_maxes <- c(beam_maxes, sum(visited))
    beam_paths[[idx]] <- visited
  }

print(max(beam_maxes))
