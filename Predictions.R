
library(caret)
library(e1071)   # for svm()

library(rpart)


diam <- read.csv(
  "E:\\Semester 5\\Big Data\\Project\\Diamonds_Final_Cleaned.csv",
  stringsAsFactors = FALSE
)

diam$cut     <- as.factor(diam$cut)
diam$color   <- as.factor(diam$color)
diam$clarity <- as.factor(diam$clarity)

#Create volume if missing
if (!"volume" %in% names(diam)) {
  diam$volume <- diam$length.mm. * diam$width.mm. * diam$height.mm.
}



set.seed(123)

train_idx <- createDataPartition(diam$price, p = 0.8, list = FALSE)

train_data <- diam[train_idx, ]
test_data  <- diam[-train_idx, ]


reg_model <- lm(price ~ carat + volume + cut + color + clarity,
                data = train_data)

summary(reg_model)


reg_pred <- predict(reg_model, newdata = test_data)


errors <- test_data$price - reg_pred   # residuals

MAE  <- mean(abs(errors))
MSE  <- mean(errors^2)
RMSE <- sqrt(MSE)
R2   <- cor(test_data$price, reg_pred)^2   # like accuracy



cat("\nREGRESSION PERFORMANCE METRICS\n",
    "Accuracy (R²):", round(R2, 4), "\n",
    "MAE          :", round(MAE, 4), "\n",
    "MSE          :", round(MSE, 4), "\n",
    "RMSE         :", round(RMSE, 4), "\n")
acc_reg <- R2
acc_reg



svr_model <- svm(
  price ~ carat + volume + cut + color + clarity,
  data   = train_data,
  kernel = "radial"
)

svr_pred <- predict(svr_model, newdata = test_data)

svr_errors <- test_data$price - svr_pred

MAE_svr  <- mean(abs(svr_errors))
MSE_svr  <- mean(svr_errors^2)
RMSE_svr <- sqrt(MSE_svr)
R2_svr   <- cor(test_data$price, svr_pred)^2

cat("\nSVR (SVM REGRESSION) PERFORMANCE METRICS\n",
    "Accuracy (R²):", round(R2_svr, 4), "\n",
    "MAE          :", round(MAE_svr, 4), "\n",
    "MSE          :", round(MSE_svr, 4), "\n",
    "RMSE         :", round(RMSE_svr, 4), "\n")

acc_svr <- R2_svr
acc_svr


tree_model <- rpart(
  price ~ carat + volume + cut + color + clarity,
  data = train_data,
  method = "anova"    
)

tree_model  

tree_pred <- predict(tree_model, newdata = test_data)


errors <- test_data$price - tree_pred

MAE_tree  <- mean(abs(errors))
MSE_tree  <- mean(errors^2)
RMSE_tree <- sqrt(MSE_tree)
R2_tree   <- cor(test_data$price, tree_pred)^2


cat("\nDECISION TREE REGRESSION PERFORMANCE METRICS\n",
    "Accuracy (R²):", round(R2_tree, 4), "\n",
    "MAE          :", round(MAE_tree, 4), "\n",
    "MSE          :", round(MSE_tree, 4), "\n",
    "RMSE         :", round(RMSE_tree, 4), "\n")

# Extract accuracy alone
acc_tree <- R2_tree
acc_tree

plot(test_data$price, reg_pred,
     xlab = "Actual Price",
     ylab = "Predicted Price",
     main = "Linear Regression: Actual vs Predicted",
     pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)   # perfect prediction line

plot(test_data$price, svr_pred,
     xlab = "Actual Price",
     ylab = "Predicted Price",
     main = "SVR Regression: Actual vs Predicted",
     pch = 16, col = "purple")
abline(0, 1, col = "red", lwd = 2)

#install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(tree_model,
           main = "Decision Tree for Diamond Price")

plot(test_data$price, tree_pred,
     xlab = "Actual Price",
     ylab = "Predicted Price",
     main = "Decision Tree: Actual vs Predicted",
     pch = 16, col = "darkgreen")
abline(0, 1, col = "red", lwd = 2)




