df <- read.csv("E:\\Semester 5\\Big Data\\Project\\Diamonds Prices2022.csv")

str(df)
summary(df)

na_counts <- colSums(is.na(df))
print(na_counts)

num_cols <- names(df)[sapply(df, is.numeric)]
print(num_cols)


par(mfrow = c(3, 3))
for (col in num_cols) {
  hist(df[[col]],
       main = paste("Histogram of", col, "(RAW)"),
       xlab = col,
       col = "lightgreen",
       border = "white")
}
par(mfrow = c(1, 1))


par(mfrow = c(3, 3))
for (col in num_cols) {
  boxplot(df[[col]],
          main = paste("Boxplot of", col, "(RAW)"),
          col = "lightblue")
}
par(mfrow = c(1, 1))

#If price is numeric and exists
if ("price" %in% num_cols) {
  
  predictors <- setdiff(num_cols, "price")
  
  par(mfrow = c(3, 3))
  for (col in predictors) {
    plot(df[[col]], df$price,
         xlab = col,
         ylab = "price",
         main = paste("Price vs", col, "(RAW)"),
         pch = 16, col = rgb(0, 0, 0, 0.3))
  }
  par(mfrow = c(1, 1))
}

# Convert to factor for plotting (does not change data)
if ("cut" %in% names(df)) df$cut <- as.factor(df$cut)
if ("color" %in% names(df)) df$color <- as.factor(df$color)
if ("clarity" %in% names(df)) df$clarity <- as.factor(df$clarity)

if ("price" %in% names(df) && "cut" %in% names(df)) {
  boxplot(price ~ cut, data = df,
          main = "Price by Cut (RAW)",
          xlab = "Cut", ylab = "Price",
          col = "lightyellow")
}

if ("price" %in% names(df) && "color" %in% names(df)) {
  boxplot(price ~ color, data = df,
          main = "Price by Color (RAW)",
          xlab = "Color", ylab = "Price",
          col = "lightyellow")
}

if ("price" %in% names(df) && "clarity" %in% names(df)) {
  boxplot(price ~ clarity, data = df,
          main = "Price by Clarity (RAW)",
          xlab = "Clarity", ylab = "Price",
          col = "lightyellow")
}


# install.packages("corrplot")  
library(corrplot)

num_data <- df[, num_cols]
num_data <- na.omit(num_data)  #correlation needs complete rows

cor_mat <- cor(num_data, method = "pearson")
corrplot(cor_mat, method = "color", type = "upper",
         tl.cex = 0.8, tl.col = "black",
         title = "Correlation Heatmap (RAW)", mar = c(0,0,2,0))
