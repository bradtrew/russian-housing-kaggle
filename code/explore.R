library(FeatureHashing)
library(tidyverse)
library(xgboost)

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



# na counts per column
raw_train %>% map(is.na) %>% map(sum) %>% flatten_dbl 

nrow(raw_train)

raw_train %>% group_by(timestamp) %>% 
  summarise(count = n(),
            avg_price = mean(price_doc, na.rm=TRUE)) %>% 
  ggplot(aes(x = timestamp, y = avg_price)) + geom_line() +
  geom_smooth()




# commercial properties
raw_train %>% filter(is.na(kitch_sq)) %>% map(is.na) %>% map(sum)  

large <- raw_train %>% group_by(id) %>%  
  inner_join(mutate_all(., funs(is.na)), by ='id') 



large_macro <- macro %>% group_by(timestamp) %>%  
  inner_join(mutate_all(., funs(is.na)), by ='timestamp') 



full_data <- large %>% inner_join(large_macro, by = c('timestamp.x' = 'timestamp'))

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

full_data[is.na(full_data)] <- 0

target <- full_data$price_doc.x
id <- full_data$id

full_data$timestamp.x <- as.integer(full_data$timestamp.x)
colnames(full_data) <- paste("start",colnames(full_data))
colnames(full_data) <- gsub(" ", "", colnames(full_data))
colnames(full_data) <- gsub("\\-", "_", colnames(full_data))
colnames(full_data) <- gsub("\\+", "_", colnames(full_data))

hashed <- hashed.model.matrix(startid+startprice_doc.x+startprice_doc.y + starttimestamp.x  ~ .-1,
                              hash.size = 2^20, data = full_data)


train_inds <- sample(1:nrow(full_data), floor(0.7*nrow(full_data)))

sp_train <- hashed[train_inds,]
sp_val <- hashed[-train_inds,]
target_train <- target[train_inds]
target_val <- target[-train_inds]



dtrain <- xgb.DMatrix(sp_train, label = target_train)
dval <- xgb.DMatrix(sp_val, label = target_val)


paramz <- list(eta = 0.01,
max_depth = 10,
subsample = 0.7,
colsample_bytree=0.7,
gamma = 0.1,
min_child_weight = 3
)

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- as.numeric(sum(labels != (preds > 0)))/length(labels)
  return(list(metric = "error", value = err))
}


xgb.train(data=dtrain,
          param=paramz,nrounds=5,nthread = 4,
          objective = "reg:linear",
          eval_metric = 'auc',
          watchlist = list(eval =dval,model = dtrain))





 


