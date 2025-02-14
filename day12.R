# day 12
library(stringr)
library(dplyr)

data <- readLines("input/day12.txt")
data <- readLines("input/test.txt")

# "#" - broken
# "." - operatinoal
# "?" - not known

# given number of missing question marks and numbers in question:
# if it's a current group, the next ? has to be #

springs <- str_replace(data, " .*", "")
groups <- str_replace(data, ".* ", "")

# spring <- springs[1]
# group <- groups[1] |> str_split_1(pattern = ",") |> as.numeric()

total <- 0
for (idx in 1:length(springs)){
  print(idx)
  spring <- springs[idx]
  group <- groups[idx] |> str_split_1(pattern = ",") |> as.numeric()
  # make a regex check for each if replacements work?
  
  spring_string <- "\\.*"
  for (idx in 1:length(group)){
    if (idx < length(group)){
      spring_string <- paste0(c(spring_string, rep("#", group[idx]), "\\.+"), collapse = "")
    } else {
      spring_string <- paste0(c(spring_string, rep("#", group[idx]), "\\.*"), collapse = "")
    }
  }
  
  # get ? locations
  q_locs <- str_locate_all(spring, "\\?")[[1]][,1]
  total_springs <- sum(group)
  missing_springs <- total_springs - nrow(str_locate_all(spring, "#")[[1]])
  
  all_combinations <- combn(q_locs,missing_springs)
  
  #Return each value:
  #test_spring <- spring
  for (i in 1:ncol(all_combinations)) {
    test_spring <- spring
    combo <- all_combinations[,i]
    for (loc in combo){
      substr(test_spring, loc, loc) <- "#"
      #test_spring <- paste0(substr(test_spring, 1, loc-1), "#", substring(test_spring,loc+1))
    }
    test_spring <- str_replace_all(test_spring, "\\?", ".")
    print(test_spring)
    if (str_detect(test_spring, spring_string)){
      total <- total + 1
    }
  }
}

total

# part 2
# each section is duplicated 5 times, separated by an additiional "?"
# ASSUMPTION: that each "?" group has at least 1 broken spring "#"

# springs <- paste0(springs, "?", springs, "?", springs, "?", springs, "?", springs)
# groups <- str_replace(data, ".* ", "")
# groups <- paste0(groups, ",", groups, ",", groups, ",", groups, ",", groups)
# 
# 
# total <- 0
# for (idx in 1:length(springs)){
#   print(idx)
#   spring <- springs[idx]
#   group <- groups[idx] |> str_split_1(pattern = ",") |> as.numeric()
#   # make a regex check for each if replacements work?
#   
#   combos <- c()
#   # remove "."s at beginning
#   spring <- str_replace(spring, "^\\.*", "")
#   # condense "."s
#   while (str_detect(spring, "\\.\\.")){
#     spring <- str_replace_all(spring, "\\.\\.", "\\.")
#   }
#   
# #while (length(group) > 0){
#   # get current group of springs and question mark block
#   # and identify how many groups and how big groups need to be
#   #cur_group <- group[1]
#   
# 
#   
#   #group_length <- 0
#   while(length(group) > 0){
#     # get current set of # and ? using regex
#     cur_coords <- str_locate(spring, "^[\\?#]+\\.*")
#     cur_string <- substr(spring, cur_coords[,1], cur_coords[,2])
#     
#     cur_qs <- str_match_all(cur_string,"\\?")[[1]] |> nrow()
#     
#     # let's assume ?/# groups are maximally inclusive, i.e., fit as many groups as possible
#     cur_string <- str_replace(cur_string, "\\.$", "")
#     
#     cur_s_length <- nchar(cur_string)
#     cur_s_qs <- nrow(str_match_all(cur_string, "\\?")[[1]])
#     
#     group_length <- 0
#     if (length(group) > 1){
#       while ((group_length + group[1]) <= cur_s_length){
#         if (length(group) > 1){
#           group_length <- group_length + group[1] + 1
#           group <- group[-1]
#         } else {
#           group_length <- group_length + group[1]
#           group <- group[-1]
#         }
#         if (length(group) == 0){
#           break
#         }
#       }
#     } else if (length(group) == 1){
#       group_length <- group[1]
#       group <- group[-1]
#     }
# 
#     
#     # choose where to put the EMPTY SPACES to form the correct number of groups
#     if (cur_s_length > group_length){
#       cur_combo <- choose(cur_s_length - group_length + 1, cur_s_qs - group_length)
#     } else { # if fully accounted for
#       cur_combo <- 1
#     }
# 
# 
#     combos <- c(combos, cur_combo)
# 
#     spring <- substring(spring, cur_coords[,2]+1)
#     #group <- group[-1]
#     #print(spring)
#   }
#   combos
# }
# 
# total