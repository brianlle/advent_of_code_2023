# day 2
library(stringr)
library(dplyr)

data <- readLines("input/day2.txt")
data <- str_replace_all(data, "Game [0-9]+: ", "")
data <- str_split(data, pattern = "; ")

get_r <- function(color_string){
  red_str <- str_extract(color_string, "[0-9]+ red")
  if (is.na(red_str)){
    return(0)
  } else {
    return(red_str |>
             str_replace(" red", "") |> as.numeric())
  }
}

get_g <- function(color_string){
  grn_str <- str_extract(color_string, "[0-9]+ green")
  if (is.na(grn_str)){
    return(0)
  } else {
    return(grn_str |>
             str_replace(" green", "") |> as.numeric())
  }
}

get_b <- function(color_string){
  blu_str <- str_extract(color_string, "[0-9]+ blue")
  if (is.na(blu_str)){
    return(0)
  } else {
    return(blu_str |>
             str_replace(" blue", "") |> as.numeric())
  }
}


get_max_color <- function(color_list){
  max_r <- 0
  max_g <- 0
  max_b <- 0
  
  for (idx in 1:length(color_list)){
    max_r <- max(max_r, get_r(color_list[idx]))
    max_g <- max(max_g, get_g(color_list[idx]))
    max_b <- max(max_b, get_b(color_list[idx]))
  }
  return(c("red" = max_r, "green" = max_g, "blue" = max_b))
}

poss_games <- c()

for (idx in 1:length(data)){
  cur_color <- get_max_color(data[[idx]])
  if ((cur_color["red"] > 12) | (cur_color["green"] > 13) | (cur_color["blue"] > 14)){
    # do nothing
  } else {
    poss_games <- c(poss_games, idx)
  }
}

print(sum(poss_games))

# part 2

game_pows <- 0

for (idx in 1:length(data)){
  cur_color <- get_max_color(data[[idx]])
  cur_pow <- cur_color["red"] * cur_color["green"] * cur_color["blue"]
  game_pows <- game_pows + cur_pow
}

print(game_pows)
