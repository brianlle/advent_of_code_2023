# day 15
library(stringr)
library(dplyr)

data <- readLines("input/day15.txt") |> str_split_1(pattern = ",") #test case
#data <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" |> str_split_1(pattern = ",") #test case

convert <- function(value, char){
  value <- value + as.numeric(charToRaw(char))
  value <- value * 17
  value <- value %% 256
}

sum <- 0
for (cur_string in data){
  cur_string <- cur_string |> str_split_1(pattern = "")
  value <- 0
  for (idx in 1:length(cur_string)){
    value <- convert(value, cur_string[idx])
  }
  sum <- sum + value
}
sum

# part 2
# letters is label, symbol is operation

convert_label <- function(string){
  #string <- string |> str_replace("[-=].*", "")
  string <- string |> str_split_1(pattern = "")
  value <- 0
  for (idx in 1:length(string)){
    value <- convert(value, string[idx])
  }
  return(value)
}

boxes <- list()

for (cur_string in data){
  label <- cur_string |> str_replace("[-=].*", "")
  operation <- cur_string |> str_replace(label, "")
  if (substr(operation, 1, 1) == "="){
    value <- substring(operation, 2) |> as.numeric()
    operation <- "="
  } else {
    operation <- "-"
  }
  box <- convert_label(label) |> as.character()
  
  cur_box <- boxes[[box]]
  
  if (operation == "="){
    if (length(cur_box) == 0){
      names(value) <- label
      cur_box <- value
    } else {
      if (label %in% names(cur_box)){
        cur_box[label] <- value
      } else {
        names(value) <- label
        cur_box <- c(cur_box, value)
      }
    }
  } else if (operation == "-"){
    if (label %in% names(cur_box)){
      cur_box <- cur_box[names(cur_box) != label]
    }
  }
  boxes[[box]] <- cur_box
}

power <- 0
for (box in names(boxes)){
  cur_box <- boxes[[box]]
  if (length(cur_box) > 0){
    box_value <- 1 + as.numeric(box)
    for (idx in 1:length(cur_box)){
      power <- power + idx*as.numeric(cur_box[idx])*box_value
    }
  }
}
power
