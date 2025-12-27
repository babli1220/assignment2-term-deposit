install.packages("stargazer")
install.packages("tidyverse")
install.packages("rstatix")
install.packages("psych")

## laoding the necessary libraries 
library(readxl)
library(psych)
library(tidyverse)
library(stargazer)
library(lmtest)
library(car)
library(rstatix)
library(caret)

# 1. LOADING THE DATA
getwd()
setwd("~/Downloads/")

data <- read_excel("termsv.xlsx")
view(data)

## Initial structure and summary
str(data)
summary(data)


## Dependent variable: subscribed (yes / no)
table(data$subscribed)

# 2. IDENTIFYING DEPENDENT VARIABLE

## Convert dependent variable to factor
data$subscribed <- as.factor(data$subscribed)


# 3. DESCRIPTIVE STATISTICS
## Numeric descriptive statistics
summary(data[, c("age",
                 "contact_duration")])

describe(data[, c("age", "contact_duration")])



## Categorical variable distributions
table(data$salary_level)
table(data$prev_campaign_outcome)
table(data$has_mortgage)
table(data$subscribed)


# 4. DATA QUALITY CHECKS

## Checking missing values
colSums(is.na(data))


## Counting observations before cleaning
n_before <- nrow(data)
n_before


# 5. DATA FORMATTING

## Convert categorical variables to factors
data$salary_level <- as.factor(data$salary_level)
data$prev_campaign_outcome <- as.factor(data$prev_campaign_outcome)
data$has_mortgage <- as.factor(data$has_mortgage)


# 6. DATA CLEANING
## Age should be positive and realistic
summary(data$age)
boxplot(data$age)

data$age[data$age < 18] <- NA
data$age[data$age > 100] <- NA


## Contact duration should be positive
summary(data$contact_duration)
boxplot(data$contact_duration)

data$contact_duration[data$contact_duration <= 0] <- NA


# 7. REMOVING MISSING VALUES (ONLY FOR USED VARIABLES)


data <- subset(data, !is.na(subscribed))
data <- subset(data, !is.na(age))
data <- subset(data, !is.na(salary_level))
data <- subset(data, !is.na(contact_duration))
data <- subset(data, !is.na(prev_campaign_outcome))
data <- subset(data, !is.na(has_mortgage))


## Count observations after cleaning
n_after <- nrow(data)
n_after


## Number of observations removed
n_removed <- n_before - n_after
n_removed


# DATA CLEANING SUMMARY (FOR REPORT)

n_before
n_after
n_removed


############################################################
## DATA VISUALISATION (ggplot2)
## Visualisations Linked to Hypotheses
############################################################

## Visualization 1:
## Contact Duration vs Term Deposit Subscription

ggplot(data, aes(x = subscribed, y = contact_duration, fill = subscribed)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Viz1: Contact Duration by Subscription Outcome",
       x = "Subscribed to Term Deposit",
       y = "Contact Duration (seconds)") +
  theme_minimal() +
  theme(legend.position = "none")


## Visualization 2:
## Age Distribution by Subscription Outcome

ggplot(data, aes(x = age, fill = subscribed)) +
  geom_histogram(position = "identity", bins = 30, alpha = 0.6) +
  labs(title = "Viz2: Age Distribution by Subscription Outcome",
       x = "Age",
       y = "Frequency") +
  theme_minimal()


## Visualization 3:
## Job Type vs Subscription Proportion
ggplot(data, aes(x = occupation, fill = subscribed)) +
  geom_bar(position = "fill") +
  labs(title = "Viz3: Subscription Proportion by Occupation",
       x = "Occupation",
       y = "Proportion Subscribed") +
  theme_minimal() +
  coord_flip()

## Visualization 4:
## Euribor 3 Month Rate by Subscription Outcome 

ggplot(data, aes(x = euribor_three_mth, fill = subscribed)) +
  geom_histogram(bins = 40, alpha = 0.6) +
  labs(title = "Viz4: Euribor 3-Month Rate by Subscription Outcome",
       x = "Euribor 3-Month Interest Rate",
       y = "Frequency") +
  theme_minimal()

## Visualization 5:
## Subscription Rate by Number of Contacts

ggplot(data, aes(x = number_contacts, fill = subscribed)) +
  geom_bar(position = "fill") +
  labs(title = "Viz5: Subscription Rate by Number of Contacts",
       x = "Number of Contacts During Campaign",
       y = "Proportion Subscribed") +
  theme_minimal()


############################################################
##  MEASURES OF ASSOCIATION
## Relationship between Independent Variables and Subscription
############################################################


## NUMERIC VARIABLES vs SUBSCRIPTION
## Independent-samples t-tests


## 1. Contact Duration vs Subscription
t.test(contact_duration ~ subscribed, data = data)

## 2. Age vs Subscription
t.test(age ~ subscribed, data = data)

## 3. Euribor 3-Month Rate vs Subscription
t.test(euribor_three_mth ~ subscribed, data = data)

## 4. Number of Contacts vs Subscription
t.test(number_contacts ~ subscribed, data = data)


############################################################
## CATEGORICAL VARIABLES vs SUBSCRIPTION
## Chi-square tests of independence
############################################################

## 5. Occupation vs Subscription
occupation_table <- table(data$occupation, data$subscribed)
chisq.test(occupation_table)

## 6. Salary Level vs Subscription
salary_table <- table(data$salary_level, data$subscribed)
chisq.test(salary_table)

## 7. Previous Campaign Outcome vs Subscription
prev_table <- table(data$prev_campaign_outcome, data$subscribed)
chisq.test(prev_table)

## 8. Mortgage Status vs Subscription
mortgage_table <- table(data$has_mortgage, data$subscribed)
chisq.test(mortgage_table)


############################################################
## REGRESSION MODELLING & PREDICTION
############################################################



## Splitting the data into training and test sets
set.seed(40498012)
index <- createDataPartition(data$subscribed, p = 0.7, list = FALSE)

train <- data[index, ]
test  <- data[-index, ]

family = "binomial"



############################################################
## MODEL 1: Core Behavioural Variables
############################################################

model1 <- glm(subscribed ~ contact_duration + number_contacts,
              data = train,
              family = "binomial")

summary(model1)



############################################################
## MODEL 2: Behavioural + Demographic
############################################################

model2 <- glm(subscribed ~ contact_duration + number_contacts + age,
              data = train,
              family = "binomial")

summary(model2)



############################################################
## MODEL 3: Add Economic Environment
############################################################

model3 <- glm(subscribed ~ contact_duration + number_contacts +
                age + euribor_three_mth,
              data = train,
              family = "binomial")

summary(model3)


############################################################
## MODEL 4: Add Occupation
############################################################

model4 <- glm(subscribed ~ contact_duration + number_contacts +
                age + euribor_three_mth + occupation,
              data = train,
              family = "binomial")

summary(model4)



############################################################
## MODEL 5: Full Model
############################################################

model5 <- glm(subscribed ~ contact_duration + number_contacts +
                age + euribor_three_mth +
                occupation + salary_level + has_mortgage,
              data = train,
              family = "binomial")

summary(model5)


############################################################
## PSEUDO R-SQUARED 
############################################################

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1 - exp (-(nullDev - dev) / modelN)
  R.n <- R.cs / (1 - (exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}

## Apply pseudo R² to all models
logisticPseudoR2s(model1)
logisticPseudoR2s(model2)
logisticPseudoR2s(model3)
logisticPseudoR2s(model4)
logisticPseudoR2s(model5)



############ MODEL PREDICTIONS############


## Predict probabilities
pred_prob1 <- predict(model1, test, type = "response")
pred_prob2 <- predict(model2, test, type = "response")
pred_prob3 <- predict(model3, test, type = "response")
pred_prob4 <- predict(model4, test, type = "response")
pred_prob5 <- predict(model5, test, type = "response")



############################################################
## CONVERT PROBABILITIES TO CLASSES
############################################################

pred_class1 <- ifelse(pred_prob1 > 0.5, "yes", "no")
pred_class2 <- ifelse(pred_prob2 > 0.5, "yes", "no")
pred_class3 <- ifelse(pred_prob3 > 0.5, "yes", "no")
pred_class4 <- ifelse(pred_prob4 > 0.5, "yes", "no")
pred_class5 <- ifelse(pred_prob5 > 0.5, "yes", "no")

pred_class1 <- as.factor(pred_class1)
pred_class2 <- as.factor(pred_class2)
pred_class3 <- as.factor(pred_class3)
pred_class4 <- as.factor(pred_class4)
pred_class5 <- as.factor(pred_class5)



############################################################
## MODEL ACCURACY
############################################################

confusionMatrix(pred_class1, test$subscribed)
confusionMatrix(pred_class2, test$subscribed)
confusionMatrix(pred_class3, test$subscribed)
confusionMatrix(pred_class4, test$subscribed)
confusionMatrix(pred_class5, test$subscribed)



############################################################
## ACCURACY COMPARISON TABLE
############################################################

accuracy_results <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  Accuracy = c(
    confusionMatrix(pred_class1, test$subscribed)$overall["Accuracy"],
    confusionMatrix(pred_class2, test$subscribed)$overall["Accuracy"],
    confusionMatrix(pred_class3, test$subscribed)$overall["Accuracy"],
    confusionMatrix(pred_class4, test$subscribed)$overall["Accuracy"],
    confusionMatrix(pred_class5, test$subscribed)$overall["Accuracy"]
  )
)

accuracy_results
