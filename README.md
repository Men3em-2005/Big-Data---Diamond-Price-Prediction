### **Project Overview**
This project aims to **predict diamond prices** using regression models.  
The dataset included diamond attributes such as carat, cut, color, clarity, and physical dimensions.  

### **Data Preparation**
- Cleaned data, removed outliers, and created a **volume feature**  
- Converted categorical features (`cut`, `color`, `clarity`) to factors  
- Performed EDA: histograms, boxplots, scatter plots, correlation analysis  

### **Models Implemented**
- Linear Regression  
- Support Vector Regression (SVR)  
- Decision Tree Regression  

### **Key Results**
- Best model achieved **R² ≈ 0.95**, with low MAE and RMSE  
- Strong correlations found between price and carat, volume  
- ANOVA and Tukey tests used for categorical variables (cut, color, clarity)  

### **Technologies Used**
R | caret | e1071 | rpart | rpart.plot | corrplot | ggplot2  
