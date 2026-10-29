
rm(list = ls())
graphics.off()
cat("\014")

library(caret)
library(MASS)
library(e1071)
library(pROC)
library(randomForest)

df <- read.csv(
  file = "online_shoppers_intention.csv",
  header = TRUE
)

# Checking the dataset
dim(df)
str(df)
head(df)
colSums(is.na(df))

# Preparing categorical variables

df$Month <- factor(
  df$Month,
  levels = c(
    "Feb", "Mar", "May", "June", "Jul",
    "Aug", "Sep", "Oct", "Nov", "Dec"
  )
)

# Visitor Type
df$VisitorType <- factor(df$VisitorType)

df$OperatingSystems <- factor(df$OperatingSystems)
df$Browser <- factor(df$Browser)
df$Region <- factor(df$Region)
df$TrafficType <- factor(df$TrafficType)

# Weekend
df$Weekend <- factor(
  df$Weekend,
  levels = c(FALSE, TRUE),
  labels = c("Weekday", "Weekend")
)

# Revenue (Response Variable)
df$Revenue <- factor(
  df$Revenue,
  levels = c(FALSE, TRUE),
  labels = c("No Purchase", "Purchase")
)
str(df)
# # Figure 1: Distribution of Revenue

revenue_count <- table(df$Revenue)
revenue_percent <- prop.table(revenue_count) * 100

# Display counts and percentages
revenue_count
round(revenue_percent, 2)

bar_midpoints <- barplot(
  revenue_count,
  main = "Distribution of Online Shopping Outcomes",
  xlab = "Purchase Outcome",
  ylab = "Number of Sessions",
  ylim = c(0, max(revenue_count) * 1.15)
)

# Add counts and percentages above the bars
text(
  x = bar_midpoints,
  y = revenue_count,
  labels = paste0(
    revenue_count,
    "\n(",
    round(revenue_percent, 1),
    "%)"
  ),
  pos = 3
)
#----------------------------------------------------------
# Purchase Rate by Visitor type
visitor_table <- table(
  df$VisitorType,
  df$Revenue
)

visitor_purchase_rate <- prop.table(
  visitor_table,
  margin = 1
)[, "Purchase"] * 100

round(visitor_purchase_rate, 2)

bar_midpoints <- barplot(
  visitor_purchase_rate,
  main = "Purchase Rate by Visitor Type",
  xlab = "Visitor Type",
  ylab = "Purchase Rate (%)",
  las = 2,
  ylim = c(0, max(visitor_purchase_rate) * 1.20),
  cex.names = 0.6
)

text(
  x = bar_midpoints,
  y = visitor_purchase_rate,
  labels = paste0(round(visitor_purchase_rate, 1), "%"),
  pos = 3
)


#--------------------------------------


#Purchase Rate by Month

month_table <- table(
  df$Month,
  df$Revenue
)

month_purchase_rate <- prop.table(
  month_table,
  margin = 1
)[, "Purchase"] * 100

# Display monthly purchase rates
round(month_purchase_rate, 2)

bar_midpoints <- barplot(
  month_purchase_rate,
  main = "Purchase Rate by Month",
  xlab = "Month",
  ylab = "Purchase Rate (%)",
  las = 2,
  ylim = c(0, max(month_purchase_rate) * 1.20)
)

text(
  x = bar_midpoints,
  y = month_purchase_rate,
  labels = paste0(
    round(month_purchase_rate, 1),
    "%"
  ),
  pos = 3,
  cex = 0.8
)

#product related pages by revenue

boxplot(
  ProductRelated ~ Revenue,
  data = df,
  main = "Product-Related Pages by Purchase Outcome",
  xlab = "Purchase Outcome",
  ylab = "Number of Product-Related Pages"
)

# Focused version displaying values up to the 95th percentile
product_limit <- quantile(
  df$ProductRelated,
  0.95,
  na.rm = TRUE
)

boxplot(
  ProductRelated ~ Revenue,
  data = df,
  main = "Product-Related Pages by Purchase Outcome",
  xlab = "Purchase Outcome",
  ylab = "Number of Product-Related Pages",
  ylim = c(0, product_limit)
)

#Product-Related Duration by Revenue
boxplot(
  ProductRelated_Duration ~ Revenue,
  data = df,
  main = "Product Browsing Duration by Purchase Outcome",
  xlab = "Purchase Outcome",
  ylab = "Product-Related Duration"
)

# Focused version displaying values up to the 95th percentile
duration_limit <- quantile(
  df$ProductRelated_Duration,
  0.95,
  na.rm = TRUE
)

boxplot(
  ProductRelated_Duration ~ Revenue,
  data = df,
  main = "Product Browsing Duration by Purchase Outcome",
  xlab = "Purchase Outcome",
  ylab = "Product-Related Duration",
  ylim = c(0, duration_limit)
)
#------------------------------------------------------
#bounce rate by revenue

boxplot(
  BounceRates ~ Revenue,
  data = df,
  main = "Bounce Rate by Purchase Outcome",
  xlab = "Purchase Outcome",
  ylab = "Bounce Rate"
)

# Focused version displaying values up to the 95th percentile
bounce_limit <- quantile(
  df$BounceRates,
  0.95,
  na.rm = TRUE
)

boxplot(
  BounceRates ~ Revenue,
  data = df,
  main = "Bounce Rate by Purchase Outcome",
  xlab = "Purchase Outcome",
  ylab = "Bounce Rate",
  ylim = c(0, bounce_limit)
)
#------------------------------------------------------
#Bounce Rate by Revenue
boxplot(
  ExitRates ~ Revenue,
  data = df,
  main = "Exit Rate by Purchase Outcome",
  xlab = "Purchase Outcome",
  ylab = "Exit Rate"
)

# Focused version displaying values up to the 95th percentile
exit_limit <- quantile(
  df$ExitRates,
  0.95,
  na.rm = TRUE
)

boxplot(
  ExitRates ~ Revenue,
  data = df,
  main = "Exit Rate by Purchase Outcome",
  xlab = "Purchase Outcome",
  ylab = "Exit Rate",
  ylim = c(0, exit_limit)
)
#--------------------------------------------------------
#page value by revenue
boxplot(
  PageValues ~ Revenue,
  data = df,
  main = "Page Value by Purchase Outcome",
  xlab = "Purchase Outcome",
  ylab = "Page Value"
)

# Focused version displaying values up to the 95th percentile
page_value_limit <- quantile(
  df$PageValues,
  0.95,
  na.rm = TRUE
)

boxplot(
  PageValues ~ Revenue,
  data = df,
  main = "Page Value by Purchase Outcome",
  xlab = "Purchase Outcome",
  ylab = "Page Value",
  ylim = c(0, page_value_limit)
)
#----------------------------------------------------------
#correlation heatmap

numeric_names <- c(
  "Administrative",
  "Administrative_Duration",
  "Informational",
  "Informational_Duration",
  "ProductRelated",
  "ProductRelated_Duration",
  "BounceRates",
  "ExitRates",
  "PageValues",
  "SpecialDay"
)

numeric_data <- df[, numeric_names]

cor_matrix <- cor(
  numeric_data,
  use = "complete.obs"
)

# Display the correlation matrix
round(cor_matrix, 2)

heatmap(
  cor_matrix,
  Rowv = NA,
  Colv = NA,
  scale = "none",
  margins = c(10, 10),
  main = "Correlation Heatmap of Numerical Predictors",
  cex.main = 0.6,
  cexRow = 0.65,
  cexCol = 0.65
  
)

#-------------------------------------------------------

#model development
############################################################

# NOTE:
# Run this once if the package is not installed:
# install.packages("klaR")

library(klaR)
# 1. Prepare data for modeling

model_df <- df

model_df$Revenue <- factor(
  model_df$Revenue,
  levels = c("No Purchase", "Purchase"),
  labels = c("NoPurchase", "Purchase")
)

table(model_df$Revenue)
prop.table(table(model_df$Revenue))


# 2. Stratified training and testing split


set.seed(7406)

train_index <- createDataPartition(
  model_df$Revenue,
  p = 0.80,
  list = FALSE
)

train_data <- model_df[train_index, ]
test_data <- model_df[-train_index, ]

# Check dataset sizes
dim(train_data)
dim(test_data)

# Check class proportions
prop.table(table(train_data$Revenue))
prop.table(table(test_data$Revenue))


# 3. Model evaluation function
#------------------------------------------------

evaluate_model <- function(
  model_name,
  actual,
  predicted,
  probability
) {

  actual <- factor(
    actual,
    levels = c("NoPurchase", "Purchase")
  )

  predicted <- factor(
    predicted,
    levels = c("NoPurchase", "Purchase")
  )

  cm <- table(
    Predicted = predicted,
    Actual = actual
  )

  TN <- cm["NoPurchase", "NoPurchase"]
  FN <- cm["NoPurchase", "Purchase"]
  FP <- cm["Purchase", "NoPurchase"]
  TP <- cm["Purchase", "Purchase"]

  accuracy <- (TP + TN) / sum(cm)

  sensitivity <- ifelse(
    TP + FN == 0,
    NA,
    TP / (TP + FN)
  )

  specificity <- ifelse(
    TN + FP == 0,
    NA,
    TN / (TN + FP)
  )

  precision <- ifelse(
    TP + FP == 0,
    NA,
    TP / (TP + FP)
  )

  f1_score <- ifelse(
    is.na(precision) |
      is.na(sensitivity) |
      precision + sensitivity == 0,
    NA,
    2 * precision * sensitivity /
      (precision + sensitivity)
  )

  roc_object <- roc(
    response = actual,
    predictor = probability,
    levels = c("NoPurchase", "Purchase"),
    direction = "<",
    quiet = TRUE
  )

  auc_value <- as.numeric(auc(roc_object))

  results <- data.frame(
    Model = model_name,
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    F1_Score = f1_score,
    AUC = auc_value
  )

  return(
    list(
      confusion_matrix = cm,
      results = results
    )
  )
}
#-----------------------------------------------------------
# 4. Prepare common data for cross-validation


cv_train <- train_data
cv_test <- test_data

cv_train$Revenue <- factor(
  cv_train$Revenue,
  levels = c("Purchase", "NoPurchase")
)

cv_test$Revenue <- factor(
  cv_test$Revenue,
  levels = c("Purchase", "NoPurchase")
)

levels(cv_train$Revenue)
table(cv_train$Revenue)
table(cv_test$Revenue)
#-----------------------------------------------------------------
# 5. Create numerical predictors for LDA, QDA, and KNN

train_predictors <- cv_train[
  ,
  names(cv_train) != "Revenue"
]

test_predictors <- cv_test[
  ,
  names(cv_test) != "Revenue"
]

# Learn dummy-variable coding from the training data only.
dummy_model <- dummyVars(
  ~ .,
  data = train_predictors,
  fullRank = TRUE
)

x_train <- predict(
  dummy_model,
  newdata = train_predictors
)

x_test <- predict(
  dummy_model,
  newdata = test_predictors
)

x_train <- as.data.frame(x_train)
x_test <- as.data.frame(x_test)

# Remove predictors with little or no variation using training data.
nzv_columns <- nearZeroVar(x_train)

if (length(nzv_columns) > 0) {

  x_train <- x_train[
    ,
    -nzv_columns,
    drop = FALSE
  ]

  x_test <- x_test[
    ,
    -nzv_columns,
    drop = FALSE
  ]
}

y_train <- cv_train$Revenue

dim(x_train)
dim(x_test)
#----------------------------------------------------------

# 6. Create identical repeated cross-validation folds


set.seed(7406)

common_folds <- createMultiFolds(
  y = y_train,
  k = 5,
  times = 3
)

common_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  index = common_folds,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter = FALSE
)
#-----------------------------------------------------
# 7. Cross-validated Logistic Regression

set.seed(7406)

logistic_model <- train(
  Revenue ~ .,
  data = cv_train,
  method = "glm",
  family = binomial,
  metric = "ROC",
  trControl = common_control
)

logistic_model$results

logistic_prediction <- predict(
  logistic_model,
  newdata = cv_test,
  type = "raw"
)

logistic_probability <- predict(
  logistic_model,
  newdata = cv_test,
  type = "prob"
)[, "Purchase"]

logistic_evaluation <- evaluate_model(
  model_name = "Logistic Regression",
  actual = cv_test$Revenue,
  predicted = logistic_prediction,
  probability = logistic_probability
)

logistic_evaluation$confusion_matrix
logistic_evaluation$results

# 8. Cross-validated Linear Discriminant Analysis


# Standard LDA has no main tuning parameter.
set.seed(7406)

lda_model <- train(
  x = x_train,
  y = y_train,
  method = "lda",
  metric = "ROC",
  trControl = common_control
)

lda_model$results

lda_prediction <- predict(
  lda_model,
  newdata = x_test,
  type = "raw"
)

lda_probability <- predict(
  lda_model,
  newdata = x_test,
  type = "prob"
)[, "Purchase"]

lda_evaluation <- evaluate_model(
  model_name = "LDA",
  actual = cv_test$Revenue,
  predicted = lda_prediction,
  probability = lda_probability
)

lda_evaluation$confusion_matrix
lda_evaluation$results

# 9. Cross-validated Quadratic Discriminant Analysis

# Standard QDA has no main tuning parameter.
set.seed(7406)

qda_model <- train(
  x = x_train,
  y = y_train,
  method = "qda",
  metric = "ROC",
  trControl = common_control
)

qda_model$results

qda_prediction <- predict(
  qda_model,
  newdata = x_test,
  type = "raw"
)

qda_probability <- predict(
  qda_model,
  newdata = x_test,
  type = "prob"
)[, "Purchase"]

qda_evaluation <- evaluate_model(
  model_name = "QDA",
  actual = cv_test$Revenue,
  predicted = qda_prediction,
  probability = qda_probability
)

qda_evaluation$confusion_matrix
qda_evaluation$results



# 10. Tuned Naive Bayes


# fL: Laplace smoothing
# usekernel: kernel density estimation
# adjust: bandwidth adjustment
nb_grid <- expand.grid(
  fL = c(0, 1),
  usekernel = c(FALSE, TRUE),
  adjust = c(0.5, 1, 2)
)

set.seed(7406)

nb_model <- train(
  Revenue ~ .,
  data = cv_train,
  method = "nb",
  metric = "ROC",
  tuneGrid = nb_grid,
  trControl = common_control
)

nb_model$bestTune
nb_model$results

nb_prediction <- predict(
  nb_model,
  newdata = cv_test,
  type = "raw"
)

nb_probability <- predict(
  nb_model,
  newdata = cv_test,
  type = "prob"
)[, "Purchase"]

nb_evaluation <- evaluate_model(
  model_name = "Naive Bayes",
  actual = cv_test$Revenue,
  predicted = nb_prediction,
  probability = nb_probability
)

nb_evaluation$confusion_matrix
nb_evaluation$results

# 11. Tuned K-Nearest Neighbors


# Scaling is performed inside train() during resampling.
knn_grid <- expand.grid(
  k = seq(3, 31, by = 2)
)

set.seed(7406)

knn_model <- train(
  x = x_train,
  y = y_train,
  method = "knn",
  metric = "ROC",
  tuneGrid = knn_grid,
  preProcess = c("center", "scale"),
  trControl = common_control
)

knn_model$bestTune
knn_model$results

knn_prediction <- predict(
  knn_model,
  newdata = x_test,
  type = "raw"
)

knn_probability <- predict(
  knn_model,
  newdata = x_test,
  type = "prob"
)[, "Purchase"]

knn_evaluation <- evaluate_model(
  model_name = "KNN",
  actual = cv_test$Revenue,
  predicted = knn_prediction,
  probability = knn_probability
)

knn_evaluation$confusion_matrix
knn_evaluation$results


# 12. Tuned Random Forest


rf_grid <- expand.grid(
  mtry = c(4, 6, 8, 10, 12, 14, 16)
)

set.seed(7406)

rf_model <- train(
  Revenue ~ .,
  data = cv_train,
  method = "rf",
  metric = "ROC",
  tuneGrid = rf_grid,
  trControl = common_control,
  ntree = 500,
  importance = TRUE
)

rf_model$bestTune
rf_model$results

plot(
  rf_model,
  main = "Random Forest Parameter Tuning"
)

rf_prediction <- predict(
  rf_model,
  newdata = cv_test,
  type = "raw"
)

rf_probability <- predict(
  rf_model,
  newdata = cv_test,
  type = "prob"
)[, "Purchase"]

rf_evaluation <- evaluate_model(
  model_name = "Random Forest",
  actual = cv_test$Revenue,
  predicted = rf_prediction,
  probability = rf_probability
)

rf_evaluation$confusion_matrix
rf_evaluation$results



# 13. Random Forest variable importance


rf_importance <- varImp(
  rf_model,
  scale = TRUE
)

rf_importance

plot(
  rf_importance,
  top = 10,
  main = "Top Predictors of Online Purchase Intention"
)

rf_importance_table <- as.data.frame(
  rf_importance$importance
)

rf_importance_table$Variable <- rownames(
  rf_importance_table
)

importance_column <- if (
  "Overall" %in% colnames(rf_importance_table)
) {
  "Overall"
} else if (
  "Purchase" %in% colnames(rf_importance_table)
) {
  "Purchase"
} else {
  colnames(rf_importance_table)[1]
}

rf_importance_table <- rf_importance_table[
  order(
    rf_importance_table[[importance_column]],
    decreasing = TRUE
  ),
]

top10_rf_importance <- head(
  rf_importance_table,
  10
)

rownames(top10_rf_importance) <- NULL

top10_rf_importance



# 14. Compare cross-validation performance


cv_model_comparison <- resamples(
  list(
    Logistic = logistic_model,
    LDA = lda_model,
    QDA = qda_model,
    NaiveBayes = nb_model,
    KNN = knn_model,
    RandomForest = rf_model
  )
)

summary(cv_model_comparison)

dotplot(
  cv_model_comparison,
  metric = "ROC",
  main = "Cross-Validated AUC by Model"
)

cv_summary <- rbind(
  data.frame(
    Model = "Logistic Regression",
    getTrainPerf(logistic_model)
  ),
  data.frame(
    Model = "LDA",
    getTrainPerf(lda_model)
  ),
  data.frame(
    Model = "QDA",
    getTrainPerf(qda_model)
  ),
  data.frame(
    Model = "Naive Bayes",
    getTrainPerf(nb_model)
  ),
  data.frame(
    Model = "KNN",
    getTrainPerf(knn_model)
  ),
  data.frame(
    Model = "Random Forest",
    getTrainPerf(rf_model)
  )
)

cv_summary



# 15. Final independent test-set comparison


final_model_results <- rbind(
  logistic_evaluation$results,
  lda_evaluation$results,
  qda_evaluation$results,
  nb_evaluation$results,
  knn_evaluation$results,
  rf_evaluation$results
)

final_model_results[, -1] <- round(
  final_model_results[, -1],
  4
)

final_model_results



# 16. Important final outputs


nb_model$bestTune
knn_model$bestTune
rf_model$bestTune

cv_summary

logistic_evaluation$confusion_matrix
lda_evaluation$confusion_matrix
qda_evaluation$confusion_matrix
nb_evaluation$confusion_matrix
knn_evaluation$confusion_matrix
rf_evaluation$confusion_matrix

top10_rf_importance
final_model_results





sessionInfo()
