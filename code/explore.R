library(tidyverse)
setwd("~/Documents/Kaggle/russian-housing-kaggle/")


raw_train <- read_csv("input/train.csv.zip")
raw_train$num_room <- as.integer(raw_train$num_room) 
raw_train$max_floor <- as.integer(raw_train$max_floor) 
raw_train$kitch_sq <- as.numeric(raw_train$kitch_sq) 

raw_test <- read_csv("input/test.csv.zip")
macro <- read_csv("input/macro.csv.zip")

#
setdiff(colnames(raw_train), colnames(raw_test))

# going to focus on a subset of the available columns that are unique to 
# a property (not about area etc)
# price_doc: sale price (this is the target variable)
# id: transaction id
# timestamp: date of transaction
# full_sq: total area in square meters, including loggias, balconies and 
#   other non-residential areas
# life_sq: living area in square meters, excluding loggias, balconies and
#   other non-residential areas
# floor: for apartments, floor of the building
# max_floor: number of floors in the building
# material: wall material
# build_year: year built
# num_room: number of living rooms
# kitch_sq: kitchen area
# state: apartment condition
# product_type: owner-occupier purchase or investment
# sub_area: name of the district


small_train <- raw_train %>% select(price_doc, id, timestamp, full_sq, life_sq, floor,
                     max_floor, material, build_year, num_room, kitch_sq,
                     state, product_type, sub_area) 
  

# seem to be lots of values that are NA, consider why this might be the case?
# also some junk years in there (0,1,20052009)
ggplot(small_train, aes(x = build_year, y=log(price_doc))) + 
  geom_point(alpha=0.1, position = "jitter") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(small_train, aes(x = build_year, y=price_doc)) + 
  geom_point(alpha=0.1, position = "jitter") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))




# linear relationship between full_sq and price_doc - makes sense
ggplot(small_train, aes(x = log(full_sq), y = log(price_doc))) + 
  geom_point(alpha=0.1, position = "jitter") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# linear relationship between life_sq and price_doc - makes sense
ggplot(small_train, aes(x = log(life_sq), y = log(price_doc))) + 
  geom_point(alpha=0.1, position = "jitter") +
  theme_minimal() 

# note the long AF tails
ggplot(small_train, aes(x = material, y = price_doc)) + 
  geom_violin(draw_quantiles = 0.5) +
  theme_minimal() 

# note the long AF tails
ggplot(small_train, aes(x = log(kitch_sq), y = log(price_doc))) + 
  geom_point(alpha=0.1, position = "jitter") +  theme_minimal() 


# mo' rooms mo' money 
ggplot(small_train, aes(x = as.factor(num_room), y = price_doc)) + 
  geom_boxplot() +
  theme_minimal() 


# not really sure what to make of the two below (especially max floor)
# how many different buildings have 47 floors? could have been 1 building
# with like 1k luxury apartments
ggplot(small_train, aes(x = as.factor(floor), y = price_doc)) + geom_boxplot() +
  theme_minimal() 
ggplot(small_train, aes(x = as.factor(max_floor), y = price_doc)) + 
  geom_boxplot() +
  theme_minimal() 





