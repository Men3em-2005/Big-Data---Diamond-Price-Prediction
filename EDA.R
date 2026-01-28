df_clean<-read.csv("E:\\Semester 5\\Big Data\\Project\\Diamonds_Final_Cleaned.csv")
df_clean

hist(df_clean$price,
     main = "Distribution of Diamond Prices",
     xlab = "Price",
     col = "lightblue", breaks = 50,xlim=c(0,12000))

hist(df_clean$carat,
     main = "Distribution of Carat",
     xlab = "Carat Weight",
     col = "lightgreen", breaks = 50,xlim=c(0.0,2.0))

hist(df_clean$volume,
     main = "Distribution of Volume",
     xlab = "Volume (mm^3)",
     col = "lightpink", breaks = 50,xlim=c(0, 350))


plot(df_clean$carat, df_clean$price,
     xlab = "Carat", ylab = "Price",
     main = "Price vs Carat",
     col = "blue", pch = 16)

plot(df_clean$volume, df_clean$price,
     xlab = "Volume", ylab = "Price",
     main = "Price vs Volume",
     col = "darkgreen", pch = 16)

plot(df_clean$depth, df_clean$price,
     xlab = "Depth", ylab = "Price",
     main = "Price vs Depth",
     col = "red", pch = 16)

plot(df_clean$table, df_clean$price,
     xlab = "Table", ylab = "Price",
     main = "Scatter plot of Price vs Table")

boxplot(price ~ cut, data = df_clean,
        col = "lightblue", main = "Price by Cut")

boxplot(price ~ color, data = df_clean,
        col = "orange", main = "Price by Color")

boxplot(price ~ clarity, data = df_clean,
        col = "lightgreen", main = "Price by Clarity")

library(corrplot)

numeric_cols <- df_clean[, sapply(df_clean, is.numeric)]
cor_mat <- cor(numeric_cols)

corrplot(cor_mat, method = "color", addCoef.col = "black",
         main = "Correlation Matrix of Numeric Features")


