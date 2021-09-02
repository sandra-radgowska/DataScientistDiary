library(tidyverse)
library(reshape2) 
library(caret) 
library(smotefamily) 
library(ROSE)
library(gridExtra) 
library(randomForest)
library(PRROC)


# Loading data
data <- data.table::fread("data/creditcard.csv")

glimpse(data)

# Checking class distribution
table(data$Class)

predictors <- select(data, -Class)

cbind(
  melt(apply(predictors, 2, min), value.name = "min"),
  melt(apply(predictors, 2, max), value.name = "max")
)

# Scaling
rescale <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

predictors_rescaled <- as.data.frame(apply(predictors, 2, rescale))

cbind(
  melt(apply(predictors_rescaled, 2, min), value.name = "min_after_rescaling"),
  melt(apply(predictors_rescaled, 2, max), value.name = "max_after_rescaling")
)

data <- cbind(Class = data$Class, predictors_rescaled)

# Sampling techniques - Performance

set.seed(23)
sample <- sample_n(data, 10000)
table(sample$Class)

# 1) generating 4 synthetic minority samples for every 1 legitimate
sample_smote <- SMOTE(X = sample[, -1], 
                      target = sample$Class, 
                      dup_size = 4)

sample_smote_data <- sample_smote$data
sample_smote_data$class <- factor(sample_smote_data$class)
levels(sample_smote_data$class)
table(sample_smote_data$class)

# 2) now randomly undersampling majority
sample_smote_under <- ovun.sample(class ~ .,
                                  data = sample_smote_data,
                                  method = "under",
                                  N = nrow(sample_smote_data[sample_smote_data$class == 1, ]) * 11)

sample_smote_under_data <- sample_smote_under$data
levels(sample_smote_under_data$class)
sample_smote_under_data$class <- relevel(sample_smote_under_data$class, ref = 1)
table(sample_smote_under_data$class)

# Changes visualization
p1 <- ggplot(sample, aes(x = V1, y = V2, col = Class)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~Class, labeller = labeller(Class = c("1" = "Fraud", "0" = "Not Fraud"))) +
  labs(
    title = "Before SMOTE",
    subtitle = "10,000 Random Sample",
    col = "Class"
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none")

p2 <- ggplot(sample_smote_data, aes(x = V1, y = V2, col = class)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~class, labeller = labeller(class = c("1" = "Fraud", "0" = "Not Fraud"))) +
  labs(
    title = "After SMOTE",
    subtitle = "4 Synthetic Majority Samples (per original minority sample)",
    col = "Class"
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none")

p3 <- ggplot(sample_smote_under_data, aes(x = V1, y = V2, col = class)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~class, labeller = labeller(class = c("1" = "Fraud", "0" = "Not Fraud"))) +
  labs(
    title = "After SMOTE & Random Majority Undersampling",
    subtitle = "Reduced majority:minority ratio to 10:1",
    col = "Class"
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none")

grid.arrange(p1, p2, p3, nrow = 3)

# Sampling techniques - performance

# 1. The unaltered, highly class-imbalanced dataset
set.seed(23)

train_index <- createDataPartition(data$Class, p=0.75, list=FALSE)

train <- data[train_index, ] # training data (75% of data)
test <- data[-train_index, ] # testing data (25% of data)

#2. A balanced dataset with less up-sampling
set.seed(23)

smote_v1 <- SMOTE(X = train[, -1], target = train$Class, dup_size = 6) # generating 4 synthetic minority samples for every 1 legitimate
smote_train_v1 <- smote_v1$data %>% rename(Class = class)

# under-sample until majority sample size matches
under_v1 <- ovun.sample(Class ~ .,
                        data = smote_train_v1,
                        method = "under",
                        N = 2 * sum(smote_train_v1$Class == 1))

train_v1 <- under_v1$data

# 3. A balanced dataset with more up-sampling
set.seed(23)

smote_v2 <- SMOTE(X = train[, -1], target = train$Class, dup_size = 29)
smote_train_v2 <- smote_v2$data %>% rename(Class = class)

under_v2 <- ovun.sample(Class ~ .,
                        data = smote_train_v2,
                        method = "under",
                        N = 2 * sum(smote_train_v2$Class == 1))

train_v2 <- under_v2$data


# 4.  A fraud-majority dataset with less up-sampling
set.seed(23)

smote_v3 <- SMOTE(X = train[, -1], target = train$Class, dup_size = 6)
smote_train_v3 <- smote_v3$data %>% rename(Class = class)

under_v3 <- ovun.sample(Class ~ .,
                        data = smote_train_v3,
                        method = "under",
                        N = round(sum(smote_train_v3$Class == 1) * (4/3)))

train_v3 <- under_v3$data

# 5. A fraud-majority dataset with more up-sampling
set.seed(23)

smote_v4 <- SMOTE(X = train[, -1], target = train$Class, dup_size = 29)
smote_train_v4 <- smote_v4$data %>% rename(Class = class)

under_v4 <- ovun.sample(Class ~ .,
                        data = smote_train_v4,
                        method = "under",
                        N = round(sum(smote_train_v4$Class == 1) * (4/3)))

train_v4 <- under_v4$data

# 6. A fraud-minority dataset with less up-sampling
set.seed(23)

smote_v5 <- SMOTE(X = train[, -1], target = train$Class, dup_size = 6)
smote_train_v5 <- smote_v5$data %>% rename(Class = class)

under_v5 <- ovun.sample(Class ~ .,
                        data = smote_train_v5,
                        method = "under",
                        N = (sum(smote_train_v5$Class == 1) * 4))

train_v5 <- under_v5$data

# 7. A fraud-minority dataset with more up-sampling
set.seed(23)

smote_v6 <- SMOTE(X = train[, -1], target = train$Class, dup_size = 29)
smote_train_v6 <- smote_v6$data %>% rename(Class = class)

under_v6 <- ovun.sample(Class ~ .,
                        data = smote_train_v6,
                        method = "under",
                        N = (sum(smote_train_v6$Class == 1) * 4))

train_v6 <- under_v6$data


# Class correction
train$Class <- factor(train$Class)
train_v1$Class <- factor(train_v1$Class)
train_v2$Class <- factor(train_v2$Class)
train_v3$Class <- factor(train_v3$Class)
train_v4$Class <- factor(train_v4$Class)
train_v5$Class <- factor(train_v5$Class)
train_v6$Class <- factor(train_v6$Class)

# All together
train_datasets <- list(
  train = train,
  train_v1 = train_v1,
  train_v2 = train_v2,
  train_v3 = train_v3,
  train_v4 = train_v4,
  train_v5 = train_v5,
  train_v6 = train_v6
)

dataset <- 0
obs <- 0
frauds <- 0
frauds_perc <- 0


for (i in 1:7) {
  dataset[i] <- names(train_datasets)[i]
  obs[i] <- nrow(train_datasets[[i]])
  frauds[i] <- sum(train_datasets[[i]]$Class == 1)
  frauds_perc[i] <- frauds[i] / obs[i]
}

(train_datasets_summary <- data.frame(
  name = dataset,
  num_obs = obs,
  frauds = frauds,
  frauds_perc = frauds_perc,
  weighting = c("original (very imbalanced)", "balanced", "balanced", "mostly fraud", "mostly fraud", "mostly non-fraud", "mostly non-fraud"),
  smote_amt = c("none", "some", "lots", "some", "lots", "some", "lots")
))

# DATA MODELING -------- Dopracować parametry modelu! 
# Prep for Random Forest

# ‘train’ - Full Dataset

table(train$Class)

rf.model <- randomForest(Class ~ ., data = train, 
                          ntree = 500,
                          mtry = 5)
print(rf.model)


rf.predict <- predict(rf.model, test)

test$Class <- as.factor(test$Class)

confusionMatrix(test$Class, rf.predict)

plot(rf.model)

# ‘train_v1’ - Small, Balanced (50:50)
table(train_v1$Class)

rf.model_v1 <- randomForest(Class ~ ., data = train_v1,
                            ntrees = 500,
                            mtry = 5)

rf.predict_v1 <- predict(rf.model_v1, test)

test$Class <- as.factor(test$Class)

confusionMatrix(test$Class, rf.predict_v1)

# ‘train_v2’ - Larger, Balanced (50:50)
table(train_v2$Class)

rf.model_v2 <- randomForest(Class ~ ., data = train_v2, 
                            ntrees = 500,
                            mtry = 5)

rf.predict_v2 <- predict(rf.model_v2, test)

confusionMatrix(test$Class, rf.predict_v2)

# ‘train_v3’ - Small, Fraud-Majority (75:25)
table(train_v3$Class)

rf.model_v3 <- randomForest(Class ~ ., data = train_v3, 
                            ntrees = 500,
                            mtry = 5)

rf.predict_v3 <- predict(rf.model_v3, test)

confusionMatrix(test$Class, rf.predict_v3)

#  ‘train_v4’ - Larger, Fraud-Majority (75:25)
table(train_v4$Class)

rf.model_v4 <- randomForest(Class ~ ., data = train_v4, 
                            ntrees = 500,
                            mtry = 5)

rf.predict_v4 <- predict(rf.model_v4, test)

confusionMatrix(test$Class, rf.predict_v4)

# ‘train_v5’ - Smaller, Fraud-Minority (25:75)
table(train_v5$Class)

rf.model_v5 <- randomForest(Class ~ ., data = train_v5, 
                            ntrees = 500,
                            mtry = 5)

rf.predict_v5 <- predict(rf.model_v5, test)

confusionMatrix(test$Class, rf.predict_v5)

# ‘train_v6’ - Larger, Fraud-Minority (25:75)
table(train_v6$Class)

rf.model_v6 <- randomForest(Class ~ ., data = train_v6, 
                            ntrees = 500,
                            mtry = 5)

rf.predict_v6 <- predict(rf.model_v6, test)

confusionMatrix(test$Class, rf.predict_v6)

