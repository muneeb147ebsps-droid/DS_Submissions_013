
set.seed(123)


# 1. Create simple synthetic dataset

n <- 500

id <- 1:n
age <- sample(18:65, n, replace = TRUE)
income <- round(rnorm(n, mean = 50000, sd = 15000), 0)
score <- round(rnorm(n, mean = 75, sd = 10), 1)
visits <- rpois(n, lambda = 4)
gender <- sample(c('Male', 'Female'), n, replace = TRUE)
region <- sample(c('North', 'South', 'East', 'West'), n, replace = TRUE)
plan <- sample(c('Basic', 'Standard', 'Premium'), n, replace = TRUE)
churn <- sample(c(0,1), n, replace = TRUE, prob = c(0.8,0.2))
date_joined <- sample(seq(as.Date('2022-01-01'), as.Date('2024-12-31'), by = 'day'), n, replace = TRUE)

# Combine into a dataframe
df <- data.frame(id, age, income, score, visits, gender, region, plan, churn, date_joined)

# Introduce some missing values
df$income[sample(1:n, 20)] <- NA
df$score[sample(1:n, 15)] <- NA


# 2. Data Cleaning


# Check structure
str(df)

# Remove duplicates
df <- unique(df)

# Handle missing values (impute with mean)
df$income[is.na(df$income)] <- mean(df$income, na.rm = TRUE)
df$score[is.na(df$score)] <- mean(df$score, na.rm = TRUE)

# Convert columns to factors
df$gender <- as.factor(df$gender)
df$region <- as.factor(df$region)
df$plan <- as.factor(df$plan)
df$churn <- as.factor(df$churn)

# Check summary
summary(df)

# 3. Exploratory Data Analysis (EDA)

library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)

# Univariate Analysis

# Bar plot for categorical variables
 {
  ggplot(df, aes_string(x = col)) + geom_bar(fill = 'lightgreen', color = 'black') +
    ggtitle(paste('Count of', col)) + theme_minimal()
}


# 4. Bivariate Analysis

# Scatterplot between income and score
ggplot(df, aes(x = income, y = score)) + geom_point(color = 'steelblue') + geom_smooth(method = 'lm', se = FALSE) +
  ggtitle('Income vs Score') + theme_minimal()

# Boxplot of income by plan
ggplot(df, aes(x = plan, y = income, fill = plan)) + geom_boxplot() +
  ggtitle('Income by Plan') + theme_minimal()

# 5. Multivariate Analysis

# Correlation matrix for numeric variables
num_data <- df %>% select(age, income, score, visits)
cor_mat <- cor(num_data)
corrplot(cor_mat, method = 'number')

