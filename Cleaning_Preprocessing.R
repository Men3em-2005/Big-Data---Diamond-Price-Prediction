df<-read.csv("E:\\Semester 5\\Big Data\\Project\\Diamonds Prices2022.csv")
df
str(df)
summary(df)
head(df)

num_cols <- names(df)[sapply(df, is.numeric)]

outlier_counts <- data.frame(
  Column = character(),
  Lower_Outliers = integer(),
  Upper_Outliers = integer(),
  Total_Outliers = integer(),
  stringsAsFactors = FALSE
)
#
for (col in num_cols) {
  
  Q1  <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3  <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  
  lower_count <- sum(df[[col]] < lower)
  upper_count <- sum(df[[col]] > upper)
  total_count <- lower_count + upper_count
  
 
  outlier_counts <- rbind(outlier_counts,
                          data.frame(Column = col,
                                     Lower_Outliers = lower_count,
                                     Upper_Outliers = upper_count,
                                     Total_Outliers = total_count))
}

outlier_counts

df_clean <- df   # make a copy to keep original safe

for (col in num_cols) {
  
  Q1  <- quantile(df_clean[[col]], 0.25, na.rm = TRUE)
  Q3  <- quantile(df_clean[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  # Keep only rows within the IQR range
  df_clean <- df_clean[df_clean[[col]] >= lower & df_clean[[col]] <= upper, ]
}


cat("Original rows:", nrow(df), "\n")
cat("Rows after removing outliers:", nrow(df_clean), "\n")


head(df_clean)
write.csv(df_clean, 
          "E:\\Semester 5\\Big Data\\Project\\Diamonds_Prices2022_Cleaned.csv",
          row.names = FALSE)

df_clean <- df_clean[df_clean$carat > 0, ]
df_clean <- df_clean[df_clean$price > 0, ]
df_clean <- df_clean[df_clean$depth >= 40 & df_clean$depth <= 80, ]
df_clean <- df_clean[df_clean$length.mm. > 0, ]
df_clean <- df_clean[df_clean$width.mm. > 0, ]
df_clean <- df_clean[df_clean$height.mm. > 0, ]

colSums(is.na(df_clean))

df_clean$cut     <- as.factor(df_clean$cut)
df_clean$color   <- as.factor(df_clean$color)
df_clean$clarity <- as.factor(df_clean$clarity)

write.csv(df_clean, "Diamonds_Final_Cleaned.csv", row.names = FALSE)
df_clean$X <- NULL
names(df_clean)
write.csv(df_clean, 
          "E:\\Semester 5\\Big Data\\Project\\Diamonds_Final_Cleaned.csv",
          row.names = FALSE)
df_clean <- df_clean[!duplicated(df_clean), ]

write.csv(df_clean, 
          "E:\\Semester 5\\Big Data\\Project\\Diamonds_Final_Cleaned.csv",
          row.names = FALSE)

num_cols_clean <- names(df_clean)[sapply(df_clean, is.numeric)]

#Plot boxplots for each numeric column
par(mfrow = c(3, 3))  
for (col in num_cols_clean) {
  boxplot(df_clean[[col]], 
          main = paste("Boxplot of", col), 
          col = "lightblue")
}
par(mfrow = c(1, 1))   #reset layout

df_clean$volume <- df_clean$length.mm. * df_clean$width.mm. * df_clean$height.mm.
write.csv(df_clean, 
          "E:\\Semester 5\\Big Data\\Project\\Diamonds_Final_Cleaned.csv",
          row.names = FALSE)
