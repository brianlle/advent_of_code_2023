# day 8

library(dplyr)
library(stringr)

data <- read.csv("input/day8.txt", sep = " ", header = FALSE)
colnames(data) <- c("hand", "bet")

instructions <- data[1,1] |> str_split_1(pattern = "")

nodes <- data[-1,]
colnames(nodes) <- c("node", "nothing", "L", "R")
nodes$L <- str_replace(nodes$L, "\\(", "") |> str_replace(",", "")
nodes$R <- str_replace(nodes$R, "\\)", "") |> str_replace(",", "")

cur_node <- "AAA"
steps <- 0
instruction_idx <- 1

while (cur_node != "ZZZ"){
  direction <- instructions[instruction_idx]
  cur_node <- nodes[nodes$node == cur_node, direction]
  
  instruction_idx <- instruction_idx + 1
  if (instruction_idx > 293){
    instruction_idx <- instruction_idx - 293
  }
  steps <- steps + 1
  print(cur_node)
}

steps

# part 2

nodes$last_letter <- substr(nodes$node, 3, 3)

A_nodes <- nodes |> subset(nodes$last_letter == "A") |> pull(node)

for (cur_node in A_nodes){

  steps <- 0
  instruction_idx <- 1
  while (substr(cur_node, 3, 3) != "Z"){
    direction <- instructions[instruction_idx]
    cur_node <- nodes[nodes$node == cur_node, direction]
    
    instruction_idx <- instruction_idx + 1
    if (instruction_idx > 293){
      instruction_idx <- instruction_idx - 293
    }
    steps <- steps + 1
    print(cur_node)
  }
  A_node_steps <- c(A_node_steps, steps)
}

# get lowest common multiple of steps to end on "Z"
options(scipen = 999)
print(numbers::mLCM(A_node_steps))

# I can't believe this worked, that must mean each "A" node only maps to one "Z" 
# node and it's cyclical

