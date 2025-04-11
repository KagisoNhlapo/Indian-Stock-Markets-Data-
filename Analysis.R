# What is Banknifty ?
#Nifty Bank, or Bank Nifty, is an index of the most liquid and large capitalised Indian banking stocks. It provides investors with a benchmark that captures the capital market performance of Indian bank stocks. The index has 12 stocks from the banking sector.

#What is in the DataSet ?
#  This dataset comprises of historical daily chart of BankNifty movement from 2000 to all the way to 2022.

#Data Description
#The data has 9 columns (excluding the index):
  
 # Time: date
#Open: Open price for the day.
#High: Highest price for the day.
#Low: lowest price for the day.
#Close: Close price for the day.
#Weekday: 0 means Monday and 5 means Friday. This column represents which day was it.
#range_HL: Market range from high to low.
##range_OC: Market range from open to close.
#type: Market type (bullish = close>open), (bearish = close)

# Work flow: 

# 1) Load libraries 
# 2) Clean data (change the date into the right format)
# 3) Understand differences in the market when its bullish and bearish
# 4) Stats difference (t-tests maybe anova)
# 5) pairs plot for visualization
# 5) Make Market type a response (try and understand how thing change 
#    when the markets are bullish and bearish)
# 5a) moving forward model. (Just a model that factors time)
# 6) Predict 
# 7) Train ML and see if it can produce future outputs 


# 1) Load libraries

library(tidyverse)
library(car)
library(MASS)
library(readxl)
library(lubridate)
library(hms)
library(xgboost) 
library(caret)


# 2) Load data and Clean the data

banknifty <- read_excel("banknifty_data - Copy.xlsx")

# Structure 

str(banknifty$type)


# difference between groups open

bank_open <- aov(open ~ type, data = banknifty)
summary(bank_open) #different

# difference between groups closed 

bank_closed <- aov(close ~ type, data = banknifty)
summary(bank_closed) #different 

# check model assumptions 

bank_closed %>% plot()

bank_open %>% plot()


# I suspect all groups are in deep different which is as a result of the the tie value being low 

# Lets try a turkey honest significance test

# where does the difference lie ans is it significant 

# Bank open 

TukeyHSD(bank_open)

# Bank close 

TukeyHSD(bank_closed)


# There is no significant difference between bull and bear, these are virtually the same
# there is however a significant difference between Tie-bear and Tie bull. You can see 
# from the report that these are difference for each group, the same patterns are 
# for the 3 categories for the high, low, and both range variables.


# issues with multicollinearity 
# but first we clean the data so time data
# rename the first column 
# then split the date and the time 
# then move the stats

# Change name  
banknifty <- banknifty %>%
  rename("Day" = "...1")

# Split columns
banknifty <- banknifty %>%
  separate(time, into = c("date", "time"), sep = " ")

# check the structure
str(banknifty)

# change the columns into appropriate structure
banknifty$date = as.Date(banknifty$date)

# Change time 
banknifty$time = as_hms(banknifty$time)

# check the structure 
str(banknifty$time)

# Make weekday categorical 

banknifty$weekday <- as.factor(banknifty$weekday)

# response change

banknifty$type <- as.factor(banknifty$type)

# Rename the factor levels
levels(banknifty$type) <- c("0", "1", "2")



# Pairs plot 

pairs(~Day + open + high + low + 
        close  + range_OC + range_HL+ 
        range_OC + weekday + time + date, data = banknifty)

# We keep = Open, range HL, weekday, time - response will be type 

# Now we will fit a logistic models 

# response structure 

str(banknifty$type)

levels(banknifty$type)

unique(banknifty$type)

# Now we need to standardize the covariates 
# Scale selected columns and add them back to the original banknifty dataset

banknifty_scaled <- banknifty %>%
  mutate(
    open = scale(open)[, 1],  # Scale 'open' and return as a vector
    range_HL = scale(range_HL)[, 1],  # Scale 'range_HL' and return as a vector
    time = scale(time)[, 1]  # Scale 'time' and return as a vector
  )

# fit ordinal model 
model_1 <- polr(type ~ open + 
                  range_HL + 
                  weekday + 
                  time, 
                data = banknifty_scaled, method = "logistic")


# summary

summary(model_1)


# fit with time 

model_2 <- polr(type ~ time, data = banknifty_scaled, method = "logistic")

# fit with weekday 

model_3 <- polr(type ~ weekday, data = banknifty_scaled, method = "logistic")

#fit with range hl

model_4 <- polr(type ~ range_HL, data = banknifty_scaled, method = "logistic")

#fit open 

model_5 <- polr(type ~ open, data = banknifty_scaled, method = "logistic")

AIC(model_1, model_2, model_3, model_4, model_5)

#Best model model with all covariates 

# Predict the response class (the most likely outcome)
pred_class <- predict(model_1, newdata = banknifty_scaled, type = "class")

# Predict probabilities for each class
pred_probs <- predict(model_1, newdata = banknifty_scaled, type = "probs")


# ML
# Im going to use ML to predict the price that stock open. I am going to do this because
# This does a better job predicting categorical variables

# 1) data processing 
# 2) Split the data into training and testing data (80%/20%)
# 3) XGBoot model 
# 4) evaluate the model
# 5) predict the opening price 


# 1) data processing 

ML_Bank_Open <- banknifty %>% 
  dplyr:: select(-Day,
                 -weekday, -time, -date)

# Using the model. matrix to create a matrix of just continuous data 
dummies <- model.matrix(type ~ . - 1, data = ML_Bank_Open)

#Traning the data 
train_idx <- createDataPartition(ML_Bank_Open$type, p = 0.8, list = FALSE)

#Training and testing
train_data <- dummies[train_idx, ]
test_data  <- dummies[-train_idx, ]

train_label <- banknifty$type[train_idx]
test_label  <- banknifty$type[-train_idx]


# Convert to xgb.DMatrix
dtrain <- xgb.DMatrix(data = train_data, label = as.numeric(train_label) - 1)
dtest  <- xgb.DMatrix(data = test_data,  label = as.numeric(test_label) - 1)

# Train model
xgb_model <- xgboost(
  data = dtrain,
  objective = "multi:softprob",
  num_class = length(levels(banknifty$type)),
  nrounds = 100,
  eval_metric = "mlogloss",
  verbose = 0
)


pred_probs <- predict(xgb_model, dtest)
pred_matrix <- matrix(pred_probs, nrow = length(levels(banknifty$type)), byrow = TRUE)
pred_classes <- max.col(t(pred_matrix))  # Get predicted class index

# Convert numeric class back to factor
pred_labels <- factor(pred_classes, labels = levels(banknifty$type))
confusionMatrix(pred_labels, test_label)


y <- 
dtrain <- xgb.DMatrix(data = train_data, label = y[train_idx])
dtest  <- xgb.DMatrix(data = test_data,  label = y[-train_idx])

xgb_model <- xgboost(
  data = dtrain,
  objective = "reg:squarederror",
  nrounds = 100
)

