library(tidyverse)
library(readxl)

sites <- c("USA", "VASB")

years <- 2020

unique_combos <- crossing(years, sites)

remmem <- as.character(unique_combos[2,2])

for(i in nrow(unique_combos)){
  
  
  
}


test <- c(1, 2, 3)
test1 <- c(3, 4, 5)
test2 <- c(2,1,1)

test_list <- 
  list(test, 
       test1, 
       test2)

i <- 3
test_list[[1 + i]] <- c(1, 1, 1)

test_list[[1 + 1]]

test_list[[4]]

names(test_list)
