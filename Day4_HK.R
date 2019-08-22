library(caTools)
library(rpart)
library(randomForest)
library(xgboost)
library(dplyr)
library(Metrics)

dataset <- read.csv('Data/train_new.csv', stringsAsFactors = T)

dataset <- dataset %>% dplyr::select(SalePrice_log, MSSubClass, MSZoning, LandSlope,
                                     X1stFlrSF, TotalBsmtSF, GarageArea, LotArea, LowQualFinSF,
                                     Neighborhood, GarageCars, YearBuilt, 
                                     GrLivArea_stand, MasVnrArea_stand, LotFrontage_log,
                                     is_Fireplace, TotalBathrooms, TotalSF_stand)


dataset$is_Fireplace <- as.factor(dataset$is_Fireplace)

set.seed(1)
split <- sample.split(dataset$SalePrice_log, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
val_set <- subset(dataset, split == FALSE)

# randomForest -----------------------------------------------------------------------
set.seed(1)
regressor <- randomForest(formula = SalePrice_log ~ .,
                          data = training_set,
                          ntree = 800)

y_pred <- predict(regressor, newdata = val_set)

rmse(val_set$SalePrice_log, y_pred)

# 0.1443787


# XGBoost  -----------------------------------------------------------------------

training_set_new <- training_set %>% dplyr::select(-SalePrice_log)
val_set_new <- val_set %>% dplyr::select(-SalePrice_log)
cat_index <- which(sapply(training_set_new, class) == "factor")
training_set_new[cat_index] <- lapply(training_set_new[cat_index], as.numeric)
val_set_new[cat_index] <- lapply(val_set_new[cat_index], as.numeric)


labels <- training_set$SalePrice_log
dtrain <- xgb.DMatrix(data = as.matrix(training_set_new),label = labels) 
dval <- xgb.DMatrix(data = as.matrix(val_set_new))


param <-list(objective = "reg:linear",
             booster = "gbtree",
             eta = 0.3,
             gamma=0,
             max_depth=6,
             min_child_weight=1,
             subsample=1,
             colsample_bytree=1)

set.seed(1)
regressor <- xgb.train(params = param, data = dtrain, nrounds = 3000)

y_pred <- predict(regressor, dval)

rmse(val_set$SalePrice_log, y_pred)

# 0.1407311
