df_clean<-read.csv("E:\\Semester 5\\Big Data\\Project\\Diamonds_Final_Cleaned.csv")
df_clean

# Step 1: Quick scatter plot (optional, for your report)
plot(df_clean$carat, df_clean$price,
     xlab = "Carat", ylab = "Price",
     main = "Scatter plot of Price vs Carat")

#NUMERICAL TESTS
cor_test_carat <- cor.test(df_clean$carat, df_clean$price, method = "pearson")
cor_test_carat #a strong relation btw carat and price

cor_test_vol<-cor.test(df_clean$volume, df_clean$price, method = "pearson")
cor_test_vol

cor_test_depth<-cor.test(df_clean$depth, df_clean$price, method = "pearson")
cor_test_depth   #remove depth during predictions
plot(df_clean$depth, df_clean$price,
     xlab = "Depth", ylab = "Price",
     main = "Scatter plot of Price vs Depth")

cor_test_table<-cor.test(df_clean$table, df_clean$price, method = "pearson")
cor_test_table  #remove table during predictions
plot(df_clean$table, df_clean$price,
     xlab = "Table", ylab = "Price",
     main = "Scatter plot of Price vs Table")

plot(df_clean$volume, df_clean$price,
     xlab = "Table", ylab = "Price",
     main = "Scatter plot of Price vs Volume")

#ANOVA TESTS
# Boxplot for visualization
boxplot(price ~ cut, data = df_clean,
        xlab = "Cut", ylab = "Price",
        main = "Price by Cut Category",
        col = "lightblue")

# ANOVA test
anova_cut <- aov(price ~ cut, data = df_clean)
summary(anova_cut)
TukeyHSD(anova_cut)


boxplot(price ~ color, data = df_clean,
        xlab = "Color", ylab = "Price",
        main = "Price by Color Category",
        col = "lightblue")

anova_color <- aov(price ~ color, data = df_clean)
summary(anova_color)
TukeyHSD(anova_color)


boxplot(price ~ clarity, data = df_clean,
        xlab = "Clarity", ylab = "Price",
        main = "Price by Clarity Category",
        col = "lightblue")

anova_clarity <- aov(price ~ clarity, data = df_clean)
summary(anova_clarity)
TukeyHSD(anova_clarity)



