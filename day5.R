# day 5
library(stringr)
library(dplyr)

data <- readLines("input/day5.txt")

# Manually parse for locations, faster than writing something up
seeds <- data[1]
seed_soil <- data[4:40]
soil_fert <- data[43:72]
fert_water <- data[75:113]
water_light <- data[116:140]
light_temp <- data[143:159]
temp_humid <- data[162:192]
humid_loc <- data[195:237]

map_number <- function(number, lines){
  number <- as.numeric(number)
  for (line in lines){
    numbers <- str_split_1(line, pattern = " ")
    map_to <- numbers[1] |> as.numeric()
    map_from <- numbers[2] |> as.numeric()
    map_range <- numbers[3] |> as.numeric()
    
    if ((number >= map_from) & (number < map_from + map_range)){
      return(map_to + number - map_from)
    }
  }
  return(number)
}

full_map <- function(number){
  number <- as.numeric(number)
  number <- number |> map_number(seed_soil) |>
    map_number(soil_fert) |>
    map_number(fert_water) |>
    map_number(water_light) |>
    map_number(light_temp) |>
    map_number(temp_humid) |>
    map_number(humid_loc)
  return(number)
}


low_number <- Inf
seeds <- str_split_1(seeds, pattern = " ")
seeds <- seeds[2:length(seeds)]
for (seed in seeds){
  cur_loc <- seed |> full_map()
  low_number <- min(low_number, cur_loc)
}

print(low_number)

# part 2

lowest_numbers <- c()
lowest_seeds <- c()
for (idx in 1:10){
  range <- seeds[2+2*(idx-1)] |> as.numeric()
  seed1 <- seeds[1+2*(idx-1)] |> as.numeric()
  seed2 <- as.numeric(seed1) + as.numeric(range)
  
  range <- floor(sqrt(range))
  cur_seed <- seed1
  low_seed <- cur_seed
  low_number <- full_map(cur_seed)
  while (cur_seed < seed2){
    cur_seed <- cur_seed + range
    cur_number <- full_map(cur_seed)
    if (cur_number < low_number){
      low_number <- cur_number
      low_seed <- cur_seed
    }
  }

  lowest_numbers <- c(lowest_numbers, low_number)
  lowest_seeds <- c(lowest_seeds, low_seed)
}

min(lowest_numbers)
lowest_seed <- lowest_seeds[lowest_numbers == min(lowest_numbers)]

for (idx in 1:10){
  range <- seeds[2+2*(idx-1)] |> as.numeric()
  seed1 <- seeds[1+2*(idx-1)] |> as.numeric()
  
  if (lowest_seed > seed1 & lowest_seed < (seed1 + range)){
    seed_range <- idx
  }
}

# let's search the full range around the number, using the jump distance
# from the prior search
search_range <- ceiling(sqrt(as.numeric(seeds[2+2*(seed_range-1)])))
cur_low <- min(lowest_numbers)
for (idx in (lowest_seed-search_range):(lowest_seed+search_range)){
  new_low <- full_map(idx)
  if (new_low < cur_low){
    cur_low <- new_low
    #print(paste(idx, cur_low))
  }
}
print(cur_low)

# not 396137072
# not 34062605
# not 34046458
# is 34039469
