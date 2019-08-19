library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(corrplot)
library(gridExtra)
library(plotly)
options(dplyr.print_max=1e9)
options(scipen=999)

train0 <- read.csv("Data/train.csv", stringsAsFactors = FALSE)

# 分割 numeric and character 欄位
num_features <- names(which(sapply(train0, is.numeric)))
cat_features <- names(which(sapply(train0, is.character)))
train_numeric <- train0[, names(train0) %in% num_features]
train_categoric <- train0[, names(train0) %in% cat_features]


missing_values <- sapply(train0, function(x) sum(is.na(x)))
null_count <- data.frame(Count = missing_values, Proportion = missing_values/nrow(train0))
null_count_gteZero <- null_count[null_count$Count > 0, ]
null_count_gteZero[order(-null_count_gteZero$Count),]

# 刪除所有出現NA的欄位
train_non_null <- train0 %>% 
  select(-c(rownames(null_count_gteZero), OverallCond, OverallQual, MSSubClass))

# 先剔除出現NA的欄位，而且是數字的欄位
match_num_features <- paste(num_features, collapse = "|")
train_non_null_df <- select(train_non_null, matches(match_num_features))





# 房屋位置與SalePrice的關係
train_SF <- select(train_non_null, 
                   matches("MSSubClass|MSZoning|LotConfig|Neighborhood|Condition1|Condition2|SalePrice"))

theme_set(theme_bw())  

train_SF %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_boxplot(na.rm = T) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))








