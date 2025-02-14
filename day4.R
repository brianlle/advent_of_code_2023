# day 4
library(stringr)
library(dplyr)

data <- readLines("input/day4.txt")
data <- str_replace(data, ".*: ", "")

points <- 0
for (card in data){
  n_wins <- 0
  
  winning_numbers <- str_extract(card, ".*\\|")
  winning_numbers <- str_replace(winning_numbers, " \\|", "")
  winning_numbers <- str_split_1(winning_numbers, pattern = " ")
  winning_numbers <- winning_numbers[winning_numbers != ""]
  held_numbers <- str_extract(card, "\\|.*")
  held_numbers <- str_replace(held_numbers, "\\| ", "")
  held_numbers <- str_split_1(held_numbers, pattern = " ")
  held_numbers <- held_numbers[held_numbers != ""]
  
  n_wins <- sum(unique(held_numbers) %in% unique(winning_numbers))
  
  if(n_wins >= 1){
    print( 2^(n_wins-1))
    points <- points + 2^(n_wins-1)
  }
}

points

# part 2

card_copies <- rep(1, length(data)) # start with 1 copy of each

for (card_idx in 1:length(data)){
  card <- data[card_idx]
  
  winning_numbers <- str_extract(card, ".*\\|")
  winning_numbers <- str_replace(winning_numbers, " \\|", "")
  winning_numbers <- str_split_1(winning_numbers, pattern = " ")
  winning_numbers <- winning_numbers[winning_numbers != ""]
  held_numbers <- str_extract(card, "\\|.*")
  held_numbers <- str_replace(held_numbers, "\\| ", "")
  held_numbers <- str_split_1(held_numbers, pattern = " ")
  held_numbers <- held_numbers[held_numbers != ""]
  
  n_wins <- sum(unique(held_numbers) %in% unique(winning_numbers))
  
  if(n_wins >= 1){
    next_card_idx <- card_idx + 1
    while(n_wins > 0){
      card_copies[next_card_idx] <- card_copies[next_card_idx] + 1*card_copies[card_idx]
      next_card_idx <- next_card_idx + 1
      n_wins <- n_wins - 1
    }
  }
}

sum(card_copies)