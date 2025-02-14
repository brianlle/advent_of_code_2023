# day 13
library(stringr)
library(dplyr)

data <- readLines("input/day13.txt")
#data <- readLines("input/test.txt")

spaces <- which(data == "")

spaces <- c(0, spaces, length(data)+1) # include 0 point for parsing, and end

rows_above <- 0
cols_left <- 0 # note; reversed from AoC framing

for (p_idx in 1:(length(spaces)-1)){
  pattern <- data[(spaces[p_idx]+1):(spaces[p_idx+1]-1)]
  
  pattern <- str_split(pattern, pattern = "") |> 
    unlist() |> 
    matrix(nrow = nchar(data[spaces[p_idx]+1]), ncol = spaces[p_idx+1] - spaces[p_idx] - 1)
  
  # check row reflections
  for (r_idx in 1:(nrow(pattern)-1)){ # reflect 1.5, 2.5, etc
    reflect <- r_idx + 0.5
    # step 1: get valid rows, recenter around reflection index, keep matching pos/neg
    rows_recentered <- reflect - 1:nrow(pattern)
    pos_rows <- rows_recentered[rows_recentered > 0]
    neg_rows <- rows_recentered[rows_recentered < 0]
    if (length(pos_rows) > length(neg_rows)){
      pos_rows <- -neg_rows
    } else if (length(neg_rows) > length(pos_rows)){
      neg_rows <- -pos_rows
    }
    # sort and convert back to og index
    neg_rows <- sort(neg_rows, decreasing = FALSE) + reflect
    pos_rows <- sort(pos_rows, decreasing = TRUE) + reflect
    
    if (identical(pattern[neg_rows,], pattern[pos_rows,])){
      rows_above <- rows_above + max(neg_rows)
    }
  }
  
  # check col reflections
  for (c_idx in 1:(ncol(pattern)-1)){ # reflect 1.5, 2.5, etc
    reflect <- c_idx + 0.5
    # step 1: get valid rows, recenter around reflection index, keep matching pos/neg
    col_recentered <- reflect - 1:ncol(pattern)
    pos_cols <- col_recentered[col_recentered > 0]
    neg_cols <- col_recentered[col_recentered < 0]
    if (length(pos_cols) > length(neg_cols)){
      pos_cols <- -neg_cols
    } else if (length(neg_cols) > length(pos_cols)){
      neg_cols <- -pos_cols
    }
    # sort and convert back to og index
    neg_cols <- sort(neg_cols, decreasing = FALSE) + reflect
    pos_cols <- sort(pos_cols, decreasing = TRUE) + reflect
    
    if (identical(pattern[,neg_cols], pattern[,pos_cols])){
      cols_left <- cols_left + max(neg_cols)
    }
  }
}

rows_above + cols_left*100

# part 2
# each pattern has 1 smudge that makes a new reflction valid (but might make the other one invalid)

switch_char <- function(char){
  if (char == "."){
    return("#")
  } else if ( char == "#"){
    return(".")
  }
}

rows_above <- 0
cols_left <- 0 # note; reversed from AoC framing

for (p_idx in 1:(length(spaces)-1)){
  pattern <- data[(spaces[p_idx]+1):(spaces[p_idx+1]-1)]
  
  pattern <- str_split(pattern, pattern = "") |> 
    unlist() |> 
    matrix(nrow = nchar(data[spaces[p_idx]+1]), ncol = spaces[p_idx+1] - spaces[p_idx] - 1)
  
  # find smudge
  smudge_looking <- TRUE
  while (smudge_looking){
    # check row reflections
    for (r_idx in 1:(nrow(pattern)-1)){ # reflect 1.5, 2.5, etc
      reflect <- r_idx + 0.5
      # step 1: get valid rows, recenter around reflection index, keep matching pos/neg
      rows_recentered <- reflect - 1:nrow(pattern)
      pos_rows <- rows_recentered[rows_recentered > 0]
      neg_rows <- rows_recentered[rows_recentered < 0]
      if (length(pos_rows) > length(neg_rows)){
        pos_rows <- -neg_rows
      } else if (length(neg_rows) > length(pos_rows)){
        neg_rows <- -pos_rows
      }
      # sort and convert back to og index
      neg_rows <- sort(neg_rows, decreasing = FALSE) + reflect
      pos_rows <- sort(pos_rows, decreasing = TRUE) + reflect
      
      neg_pattern <- pattern[neg_rows,]
      pos_pattern <- pattern[pos_rows,]
      
      for (char_idx in 1:length(neg_pattern)){
        new_neg <- neg_pattern
        new_neg[char_idx] <- switch_char(new_neg[char_idx])
        if (identical(new_neg, pos_pattern)){
          smudge_looking = FALSE
          #pattern[neg_rows,] <- new_neg
          rows_above <- rows_above + max(neg_rows)
        }
        if(!smudge_looking){
          break
        }
      }
      if(!smudge_looking){
        break
      }
    }
    
    # check col reflections
    for (c_idx in 1:(ncol(pattern)-1)){ # reflect 1.5, 2.5, etc
      reflect <- c_idx + 0.5
      # step 1: get valid rows, recenter around reflection index, keep matching pos/neg
      col_recentered <- reflect - 1:ncol(pattern)
      pos_cols <- col_recentered[col_recentered > 0]
      neg_cols <- col_recentered[col_recentered < 0]
      if (length(pos_cols) > length(neg_cols)){
        pos_cols <- -neg_cols
      } else if (length(neg_cols) > length(pos_cols)){
        neg_cols <- -pos_cols
      }
      # sort and convert back to og index
      neg_cols <- sort(neg_cols, decreasing = FALSE) + reflect
      pos_cols <- sort(pos_cols, decreasing = TRUE) + reflect
      
      neg_pattern <- pattern[,neg_cols]
      pos_pattern <- pattern[,pos_cols]
      
      for (char_idx in 1:length(neg_pattern)){
        new_neg <- neg_pattern
        new_neg[char_idx] <- switch_char(new_neg[char_idx])
        if (identical(new_neg, pos_pattern)){
          smudge_looking = FALSE
          #pattern[,neg_cols] <- new_neg
          cols_left <- cols_left + max(neg_cols)
        }
        if(!smudge_looking){
          break
        }
      }
      if(!smudge_looking){
        break
      }
    }
  }
}

rows_above + cols_left*100