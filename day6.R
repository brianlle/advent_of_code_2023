# day 6
library(stringr)
library(dplyr)

# from input
times <- c(40, 82, 84, 92)
distances <- c(233, 1011, 1110, 1487)

hold_times <- c()

for (idx in 1:length(times)){
  total_ways <- 0
  
  cur_time <- times[idx]
  cur_dist <- distances[idx]
  
  for (time_idx in 1:(cur_time-1)){
    # hold button for that time, release
    # time traveled = cur-time - time_held
    # velocity = time held
    
    distance <- time_idx * (cur_time - time_idx)
    if (distance > cur_dist){
      total_ways <- total_ways + 1
    }
  }
  
  hold_times <- c(hold_times, total_ways)
}

prod(hold_times)

# part 2
times <- as.numeric(paste0(times, collapse = ""))
distances <- as.numeric(paste0(distances, collapse = ""))

# calculate the range of times that will work, solve analytically
# x * (times - x) > distances
# -x^2 + x*times - distances > 0
# a = -1, b = times, c = - distances

root1 <- (-1*times + sqrt(times^2 - 4*distances))/-2
root2 <- (-1*times - sqrt(times^2 - 4*distances))/-2
max(root1, root2) - min(root1, root2) - 1

