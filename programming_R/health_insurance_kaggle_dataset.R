library(readr) #read in the data
library(ggplot2) #visualization 
library(corrplot) #visualization of correlation
library(ggcorrplot) #visualization of correlation
library(reshape2) #melt function
library(dplyr) #used for data transformations
library(tidyverse) #used for data transformations
library(caTools)


insurance <- read_csv("C:/Users/karth/Desktop/Dataset/insurance.csv")
View(insurance)
summary(insurance)
structure(insurance)

#missing values
any(is.na(insurance))


#converting into factor


insurance$smoker <- as.factor(insurance$smoker)
insurance$sex <- as.factor(insurance$sex)
insurance$region <- as.factor(insurance$region)


#Correlation Heatmap
insurance_corrplot <- mutate_all(insurance, funs(as.numeric))
a<-corrplot(cor(insurance_corrplot), method = "color", type = "lower")
a

#using bmi extracting obese people 
insurance <- insurance %>% mutate(obese = ifelse(bmi >= 25, 1, 0))
insurance$obese <- as.factor(insurance$obese)

#checking data normally distributed
histgram <- insurance %>% select(c(1, 3, 7)) %>% gather()
hist <- ggplot(data = ins.h, mapping = aes(x = value)) + geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
hist

library(caTools)
set.seed(123)
split = sample.split(insurance, SplitRatio = 0.80)
training_set = subset(insurance, split==TRUE)
test_set = subset(insurance, split==FALSE)

# Fitting Simple Linear Regression on the Training set
regressor = lm(formula = charges ~ obese * smoker + age + children,data = training_set)
summary(regressor)
# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred



