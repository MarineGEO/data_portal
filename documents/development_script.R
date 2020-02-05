library(tidyverse)
library(readxl)

test <- read_excel("./documents/secrets/fish_seines_USA-VASB_2019-09-23.xlsx", sheet="sample_metadata")

obs <- as.vector(test[1,], mode="list")
obs <- setNames(as.vector(as.character(test[1,])), colnames(test))

test$sample_collection_date[]

invalid_index <- which(is.na(anydate(c(test$sample_collection_date, NA, "hello", 663))))
c(test$sample_collection_date, NA, "hello", 663)[invalid_index]

sites <- c("USA", "VASB")

years <- 2020

unique_combos <- crossing(years, sites)

remmem <- as.character(unique_combos[2,2])

for(i in nrow(unique_combos)){
  
  
  
}


test <- c(1, 2, 3)
test1 <- c(3, 4, 5)
test2 <- NULL

test_list <- 
  list(test, 
       test1, 
       test2)

i <- 3
test_list[[1 + i]] <- c(1, 1, 1)

test_list[[1 + 1]]

test_list[[3]]

test_list[[4]]

names(test_list)
