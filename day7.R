# day 6
library(stringr)
library(dplyr)

data <- read.csv("input/day7.txt", sep = " ", header = FALSE)
colnames(data) <- c("hand", "bet")

get_high_card <- function(cards){
  if ("A" %in% cards){
    return("A")
  } else if ("K" %in% cards){
    return("K")
  } else if ("Q" %in% cards){
    return("Q")
  } else if ("J" %in% cards){
    return("J")
  } else if ("T" %in% cards){
    return("T")
  } else if ("9" %in% cards){
    return("9")
  } else if ("8" %in% cards){
    return("8")
  } else if ("7" %in% cards){
    return("7")
  } else if ("6" %in% cards){
    return("6")
  } else if ("5" %in% cards){
    return("5")
  } else if ("4" %in% cards){
    return("4")
  } else if ("3" %in% cards){
    return("3")
  } else if ("2" %in% cards){
    return("2")
  } else {
    print("broken")
    return("ZZZZZ")
  }
}

get_high_card_value <- function(cards){
  if ("A" %in% cards){
    return("14")
  } else if ("K" %in% cards){
    return("13")
  } else if ("Q" %in% cards){
    return("12")
  } else if ("J" %in% cards){
    return("11")
  } else if ("T" %in% cards){
    return("10")
  } else if ("9" %in% cards){
    return("09")
  } else if ("8" %in% cards){
    return("08")
  } else if ("7" %in% cards){
    return("07")
  } else if ("6" %in% cards){
    return("06")
  } else if ("5" %in% cards){
    return("05")
  } else if ("4" %in% cards){
    return("04")
  } else if ("3" %in% cards){
    return("03")
  } else if ("2" %in% cards){
    return("02")
  } else {
    print("broken")
    return("ZZZZZ")
  }
}

# give values
id_hand <- function(cards){
  if (max(table(cards)) == 5){
    return("five_kind")
  } else if (max(table(cards)) == 4){
    return("four_kind")
  } else if (max(table(cards)) == 3 & min(table(cards)) == 2){
    return("full_house")
  } else if (max(table(cards)) == 3 & min(table(cards)) == 1){
    return("three_kind")
  } else if (max(table(cards)) == 1){
    return("high_card")
  } else if (table(table(cards))[2] == 2){
    return("two_pair")
  } else if (table(table(cards))[2] == 1){
    return("one_pair")
  } else {
    print("broke")
    return("ERROR")
  }
}

for (idx in 1:nrow(data)){
  cards <- str_split(data[idx,"hand"], pattern = "")
  data[idx,"type"] <- id_hand(cards)
  print(idx)
}

all_hands_sorted <- c()

for (cur_type in c("high_card", "one_pair", "two_pair", "three_kind", "full_house", "four_kind", "five_kind")){
  cur_hands <- data |> subset(type == cur_type) |> pull(hand)
  names(cur_hands) <- as.character(data |>  subset(type == cur_type) |> pull(bet))
  
  for (bet in names(cur_hands)){
    cur_hand <- cur_hands[bet]
    cards <- str_split_1(cur_hand, pattern = "")
    new_hand <- ""
    while (length(cards) > 0){
      new_hand <- paste0(new_hand, get_high_card_value(cards[1]))
      cards <- cards[-1]
    }
    cur_hands[bet] <- new_hand
  }
  cur_hands <- sort(cur_hands) # decreasing = FALSE
  
  all_hands_sorted <- c(all_hands_sorted, cur_hands)
}

sum <- 0
for (idx in 1:length(all_hands_sorted)){
  sum <- sum + idx*as.numeric(names(all_hands_sorted)[idx])
}
print(sum)


# part 2 ----

# modify functions, move J to bottom, identify J as wildcard in hands
get_high_card <- function(cards){

  if ("A" %in% cards){
    return("A")
  } else if ("K" %in% cards){
    return("K")
  } else if ("Q" %in% cards){
    return("Q")
  } else if ("T" %in% cards){
    return("T")
  } else if ("9" %in% cards){
    return("9")
  } else if ("8" %in% cards){
    return("8")
  } else if ("7" %in% cards){
    return("7")
  } else if ("6" %in% cards){
    return("6")
  } else if ("5" %in% cards){
    return("5")
  } else if ("4" %in% cards){
    return("4")
  } else if ("3" %in% cards){
    return("3")
  } else if ("2" %in% cards){
    return("2")
  } else if ("J" %in% cards){
    return("J")
  } else {
    print("broken")
    return("ZZZZZ")
  }
}

get_high_card_value <- function(cards){
  if ("A" %in% cards){
    return("14")
  } else if ("K" %in% cards){
    return("13")
  } else if ("Q" %in% cards){
    return("12")
  } else if ("T" %in% cards){
    return("10")
  } else if ("9" %in% cards){
    return("09")
  } else if ("8" %in% cards){
    return("08")
  } else if ("7" %in% cards){
    return("07")
  } else if ("6" %in% cards){
    return("06")
  } else if ("5" %in% cards){
    return("05")
  } else if ("4" %in% cards){
    return("04")
  } else if ("3" %in% cards){
    return("03")
  } else if ("2" %in% cards){
    return("02")
  } else if ("J" %in% cards){
    return("01")
  } else {
    print("broken")
    return("ZZZZZ")
  }
}

# need to update id_hand to identify hands if they have a J
id_hand <- function(cards){
  if ("J" %in% cards){
    no_J_cards <- cards[cards != "J"]
    max_cards <- names(table(no_J_cards)[table(no_J_cards) == max(table(no_J_cards))])
    cards[cards == "J"] <- get_high_card(max_cards)
  }
  
  if (max(table(cards)) == 5){
    return("five_kind")
  } else if (max(table(cards)) == 4){
    return("four_kind")
  } else if (max(table(cards)) == 3 & min(table(cards)) == 2){
    return("full_house")
  } else if (max(table(cards)) == 3 & min(table(cards)) == 1){
    return("three_kind")
  } else if (max(table(cards)) == 1){
    return("high_card")
  } else if (table(table(cards))[2] == 2){
    return("two_pair")
  } else if (table(table(cards))[2] == 1){
    return("one_pair")
  } else {
    print("broke")
    return("ERROR")
  }
}

for (idx in 1:nrow(data)){
  cards <- str_split_1(data[idx,"hand"], pattern = "")
  data[idx,"type"] <- id_hand(cards)
  print(idx)
}

all_hands_sorted <- c()

for (cur_type in c("high_card", "one_pair", "two_pair", "three_kind", "full_house", "four_kind", "five_kind")){
  cur_hands <- data |> subset(type == cur_type) |> pull(hand)
  names(cur_hands) <- as.character(data |>  subset(type == cur_type) |> pull(bet))
  
  for (bet in names(cur_hands)){
    cur_hand <- cur_hands[bet]
    cards <- str_split_1(cur_hand, pattern = "")
    new_hand <- ""
    while (length(cards) > 0){
      new_hand <- paste0(new_hand, get_high_card_value(cards[1]))
      cards <- cards[-1]
    }
    cur_hands[bet] <- new_hand
  }
  cur_hands <- sort(cur_hands) # decreasing = FALSE
  
  all_hands_sorted <- c(all_hands_sorted, cur_hands)
}

sum <- 0
for (idx in 1:length(all_hands_sorted)){
  sum <- sum + idx*as.numeric(names(all_hands_sorted)[idx])
}
print(sum)