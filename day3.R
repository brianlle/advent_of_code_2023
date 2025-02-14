# day 3
library(stringr)
library(dplyr)

data <- readLines("input/day3.txt")
data <- str_split(data, pattern = "")

data[[1]][2]
numbers <- as.character(0:9)
numbers_dot <- c(numbers, ".")

# find all numbers and store positions
x <- 1
y <- 1
number_pos <- list()
for (y in 1:length(data)){
  print(y)
  for (x in 1:length(data[[1]])){
    if (data[[y]][x] %in% numbers){
      # do stuff only if it's start of number
      do_stuff = FALSE
      if (x == 1){do_stuff = TRUE}
      if (x > 1){
        if (data[[y]][x-1] %in% numbers){
          do_stuff = FALSE
        } else {
          do_stuff = TRUE
        }
      }
      if (do_stuff){ # cover edge case of number at left edge
        x_max = x
        while(data[[y]][x_max] %in% numbers & x_max <= length(data[[1]])){ # case of right edge
          x_max <- x_max + 1
        }
        
        # find if there's a symbol adjacent
        x_min_search <- max(1, x-1)
        x_max_search <- min(x_max-1+1, length(data[[1]]))
        y_min_search <- max(1, y-1)
        y_max_search <- min(y+1, length(data))
        
        symbol <- FALSE
        
        for (y_search in y_min_search:y_max_search){
          if (str_detect(paste(data[[y_search]][x_min_search:x_max_search], collapse = ""), "[^\\.0123456789]")){
            symbol <- TRUE
          }
        }
        
        number_pos <- c(number_pos, list(c("number" = as.numeric(paste(data[[y]][x:(x_max-1)], collapse = "")),
                                          "x" = x,
                                          "x_max" = x_max-1,
                                          "y" = y,
                                          "symbol" = symbol)))
      }
    }
  }
}

part_sum <- 0
for (idx in 1:length(number_pos)){
  if (number_pos[[idx]]["symbol"] == 1){
    part_sum <- part_sum + number_pos[[idx]]["number"]
  }
}
part_sum


# part 2
# gear: "*" adjacent to 2 numbers

# modify p1: same stuff to find number, but then only look to see if number
# is next to a *, and if yes, store star and store number
x <- 1
y <- 1
star_list <- list()
for (y in 1:length(data)){
  print(y)
  for (x in 1:length(data[[1]])){
    if (data[[y]][x] %in% numbers){
      # do stuff only if it's start of number
      do_stuff = FALSE
      if (x == 1){do_stuff = TRUE}
      if (x > 1){
        if (data[[y]][x-1] %in% numbers){
          do_stuff = FALSE
        } else {
          do_stuff = TRUE
        }
      }
      if (do_stuff){ # cover edge case of number at left edge
        x_max = x
        while(data[[y]][x_max] %in% numbers & x_max <= length(data[[1]])){ # case of right edge
          x_max <- x_max + 1
        }
        
        # find if there's a STAR adjacent
        x_min_search <- max(1, x-1)
        x_max_search <- min(x_max-1+1, length(data[[1]]))
        y_min_search <- max(1, y-1)
        y_max_search <- min(y+1, length(data))
        
        star <- FALSE
        star_x <- -1
        star_y <- -1
        
        # assume no number touches two stars...
        for (y_search in y_min_search:y_max_search){
          for (x_search in x_min_search:x_max_search){
            if (data[[y_search]][x_search] == "*"){
              star_x <- x_search
              star_y <- y_search
              star_id <- paste0("S", star_x, "-", star_y)
              
              if (star_id %in% names(star_list)){
                star_list[[star_id]] <- c(star_list[[star_id]], as.numeric(paste(data[[y]][x:(x_max-1)], collapse = "")))
              } else {
                star_list[[star_id]] <- as.numeric(paste(data[[y]][x:(x_max-1)], collapse = ""))
              }
            }
          }
        }
      }
    }
  }
}

gear_ratio_sum <- 0
for (idx in 1:length(star_list)){
  if (length(star_list[[idx]]) == 2){
    gear_ratio_sum <- gear_ratio_sum + prod(star_list[[idx]])
  }
}
