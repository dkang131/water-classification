setwd('C:/Users/darre/Desktop/SEM 7/AnDat/Post Midterm')

library(MASS)
library(ggplot2)
library(caret)
library(ResourceSelection)
library(pROC)
library(oddsratio)
library(ltm)
library(tidyverse)
library(DMwR)
library(biotools)
library(dplyr)

wq <- read.csv('waterQuality1.csv')
head(wq)
str(wq)

wq$ammonia = as.numeric(wq$ammonia)
wq$is_safe = as.numeric(wq$is_safe)
str(wq)
summary(wq)

# removing NA
sapply(wq, function(x) sum(is.na(x)))
wq = wq %>% 
  drop_na()
sapply(wq, function(x) sum(is.na(x)))

wq[,1:20] %>% summarise(across(where(is.numeric), .fns = 
                          list(min = min,
                               median = median,
                               mean = mean,
                               stdev = sd,
                               q25 = ~quantile(., 0.25),
                               q75 = ~quantile(., 0.75),
                               max = max))) %>%
  pivot_longer(everything(), names_sep='_', names_to=c('variable', '.value')) %>%
  print(n = 27)

# check multico
cor <- cor(wq)

# checking balance of y variable
table(wq$is_safe)
barplot(table(wq$is_safe), xlab = 'is_safe', ylab = 'count')

# train test split
set.seed(13)
index <- createDataPartition(wq$is_safe, p = 0.8, list = FALSE)
#index = sample(nrow(wq), 0.80 * nrow(wq))
train <- wq[index,]
test <- wq[-index,]
test$is_safe <- as.numeric(test$is_safe)

# training model 1
model1 <- glm(is_safe~., family = binomial(link = 'logit'), data = train)
summary(model1)
#or_glm(data = train, model = logistic_model, incr = list(X1 = 1, X2 = 1))

exp(cbind(OR = coef(model1), confint(model1)))

hoslem.test(model1$y, fitted(model1))

# if pval > alpha -> model sesuai

# training model 2
model2 = glm(is_safe~. -flouride -lead, data = train, family = binomial)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

h1 <- hoslem.test(model2$y, fitted(model2))
h1 

# training model 3
model3 = glm(is_safe~. -flouride -lead -barium -nitrites -bacteria, data = train, family = binomial)
summary(model3)
exp(cbind(OR = coef(model3), confint(model3)))

hoslem.test(model3$y, fitted(model3))
 

#MODEL 1
predictTest = predict(model1, type = "response", test[,-21])
p_opt <- ((7084 / (7084+912)) + 0.5)/2
predictClass = ifelse(predictTest > p_opt,1,0)
# table(test[,21],predictClass)

cm_1 <- confusionmatrix(test[,21],predictClass);cm_1
n1 = sum(cm_1)
nc1 = nrow(cm_1)
diag1 = diag(cm_1)
rowsum1 = apply(cm_1, 1, sum)
colsum1 = apply(cm_1, 2, sum)
p1 = rowsum1/n1
q1 = colsum1/n1
accuracy1 = sum(diag1)/n1
precission1 = diag1/colsum1
recall1 = diag1/rowsum1
f1_1 = 2*precission1*recall1/(precission1+recall1)
data.frame(accuracy1,precission1,recall1,f1_1)

cm1 <- confusionMatrix(as.factor(test[,21]),as.factor(predictClass));cm1
roc_lda = multiclass.roc(test$is_safe, predictClass)
auc(roc_lda)

#MODEL 2
predictTest_2 = predict(model2, type = "response", test[,-21])
p_opt <- ((7084 / (7084+912)) + 0.5)/2
predictClass_2 = ifelse(predictTest_2 > p_opt,1,0)
# table(test[,21],predictClass_2)

cm_2 <- confusionmatrix(test[,21],predictClass_2);cm_2
n2 = sum(cm_2)
nc2 = nrow(cm_2)
diag2 = diag(cm_2)
rowsum2 = apply(cm_2, 1, sum)
colsum2 = apply(cm_2, 2, sum)
p2 = rowsum2/n2
q2 = colsum2/n2
accuracy2 = sum(diag2)/n2
precission2 = diag2/colsum2
recall2 = diag2/rowsum2
f1_2 = 2*precission2*recall2/(precission2+recall2)
data.frame(accuracy2,precission2,recall2,f1_2)

cm2 <- confusionMatrix(as.factor(test[,21]),as.factor(predictClass_2));cm2
roc_lda_2 = multiclass.roc(test$is_safe, predictClass_2)
auc(roc_lda_2)

# MODEL 3
predictTest_3 = predict(model3, type = "response", test[,-21])
p_opt <- ((7084 / (7084+912)) + 0.5)/2
predictClass_3 = ifelse(predictTest_3 > p_opt,1,0)
# table(test[,21],predictClass_2)

cm_3 <- confusionmatrix(test[,21],predictClass_3);cm_3
n3 = sum(cm_3)
nc3 = nrow(cm_3)
diag3 = diag(cm_3)
rowsum3 = apply(cm_3, 1, sum)
colsum3 = apply(cm_3, 2, sum)
p3 = rowsum3/n3
q3 = colsum3/n3
accuracy3 = sum(diag3)/n3
precission3 = diag3/colsum3
recall3 = diag3/rowsum3
f1_3 = 2*precission3*recall3/(precission3+recall3)
data.frame(accuracy3,precission3,recall3,f1_3)

cm3 <- confusionMatrix(as.factor(test[,21]),as.factor(predictClass_3));cm3
roc_lda_3 = multiclass.roc(test$is_safe, predictClass_3)
auc(roc_lda_3)

# Accuracy values of 3 models above
# modelacr1
# modelacr2
# modelacr3
