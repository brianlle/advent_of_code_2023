# Day 1 ----
library(stringr)
library(dplyr)

data <- read.table("input/day1.txt")$V1

numbers <- as.character(0:9)
sum <- 0

for (idx in 1:length(data)){
  chars <- data[idx] |> str_split_1(pattern = "")
  start <- 1
  end <- length(chars)
  while(TRUE){
    if (chars[start] %in% numbers){
      break
    } else {
      start <- start + 1
    }
  }
  while(TRUE){
    if (chars[end] %in% numbers){
      break
    } else {
      end <- end - 1
    }
  }
  sum <- sum + as.numeric(paste0(chars[start], chars[end]))
}

print(sum)

## part 2
# wrong: 52963
#eightwo eighthree threeight nineight sevenine twone oneight fiveight nineight
update_number <- function(string){
  string <- string |> 
    str_replace_all("eightwo", "82") |>
    str_replace_all("eighthree", "83") |>
    str_replace_all("threeight", "38") |>
    str_replace_all("nineight", "98") |>
    str_replace_all("sevenine", "79") |>
    str_replace_all("twone", "21") |>
    str_replace_all("oneight", "18") |>
    str_replace_all("fiveight", "58") |>
    str_replace_all("nineight", "98") |>
    str_replace_all("one", "1") |>
    str_replace_all("two", "2") |>
    str_replace_all("three", "3") |>
    str_replace_all("four", "4") |>
    str_replace_all("five", "5") |>
    str_replace_all("six", "6") |>
    str_replace_all("seven", "7") |>
    str_replace_all("eight", "8") |>
    str_replace_all("nine", "9")
  
  return(string)
}

numbers <- as.character(0:9)
sum <- 0

for (idx in 1:length(data)){
  chars <- data[idx] |> update_number() |> str_split_1(pattern = "")
  start <- 1
  end <- length(chars)
  while(TRUE){
    if (chars[start] %in% numbers){
      break
    } else {
      start <- start + 1
    }
  }
  while(TRUE){
    if (chars[end] %in% numbers){
      break
    } else {
      end <- end - 1
    }
  }
 print(as.numeric(paste0(chars[start], chars[end])))
  sum <- sum + as.numeric(paste0(chars[start], chars[end]))
}

print(sum)
