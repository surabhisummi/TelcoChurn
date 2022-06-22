#load the data
rm(list=ls())
setwd("E:/2Sem_all_study_material/SDM/week8/assign7")
library(readxl)
d <- read_excel("TelcoChurn.xlsx", sheet= "Data")
colnames(d)=tolower(make.names(colnames(d)))    #convert to lowercase variable name
str(d)

#feature engineering


d$churn[d$churn == "Yes"] <- 1      # Replacing Yes by 1
d$churn[d$churn == "No"] <- 0  

d$multiplelines[d$multiplelines == "No phone service"] <- "No"
d$onlinesecurity [d$onlinesecurity == "No internet service"] <- "No"
d$onlinebackup [d$onlinebackup == "No internet service"] <- "No"
d$deviceprotection[d$deviceprotection == "No internet service"] <- "No"
d$techsupport[d$techsupport == "No internet service"] <- "No"
d$streamingtv[d$streamingtv == "No internet service"] <- "No"
d$streamingmovies[d$streamingmovies == "No internet service"] <- "No"

d$phoneservice <- ifelse(d$phoneservice=='Yes', 1,0)
d$multiplelines <- ifelse(d$multiplelines=='Yes', 1,0)
d$onlinesecurity<- ifelse(d$onlinesecurity=='Yes', 1,0)
d$onlinebackup<- ifelse(d$onlinebackup=='Yes', 1,0)
d$deviceprotection<- ifelse(d$deviceprotection=='Yes', 1,0)
d$techsupport<- ifelse(d$techsupport=='Yes', 1,0)
d$streamingtv<- ifelse(d$streamingtv=='Yes', 1,0)
d$streamingmovies<- ifelse(d$streamingmovies=='Yes', 1,0)
d$onlinebackup<- ifelse(d$onlinebackup=='Yes', 1,0)
d$paperlessbilling<- ifelse(d$paperlessbilling=='Yes', 1,0)

d$internetservice <- as.factor(d$internetservice)
d$contract <- as.factor(d$contract)
d$paymentmethod <- as.factor(d$paymentmethod)

colSums(is.na(d))                                
#d$customerid <- as.character(d$customerid)
df <- d %>% mutate(totalcharges = ifelse(is.na(d$totalcharges), 
                                              d$monthlycharges*d$tenure, totalcharges) )
colSums(is.na(df))    

#exploratory analysis 
table(df$churn)
table(df$churn,df$phoneservice)
table(df$churn,df$internetservice)

#data visualization


library(ggplot2)
library(dplyr)
library(MASS)
library(boot)
library(caret)
library(e1071)

str(df)
boxplot(df$tenure)
boxplot(df$monthlycharges)

ggplot(df, aes(x = churn))+ geom_histogram(stat = "count", fill = c("light green", "red"))

df %>% filter(df$churn == 1) %>% ggplot( aes(x=  tenure))+geom_bar(fill = "red" )

variables <- list(  'phoneservice', 'multiplelines', 'internetservice','onlinesecurity', 
                    'onlinebackup', 'deviceprotection', 'techsupport','streamingtv',
                    'streamingmovies' )
plotG <- list()
for (i in variables){
  plotG <-  ggplot(df, aes_string(x = i, fill = as.factor(df$churn)))+
    geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")
  
  print(plotG)
}

# Correlation Test

library("PerformanceAnalytics")
df_temp <- df[, c(6,19,20)]
chart.Correlation(df_temp)          #we gonna drop tenure and move forward with total changes                    

#models
df <- df[df$phoneservice == 1,] # creating new column where phoneservice is 1.
df$churn <- as.numeric(df$churn)
attach(df)
logit1 <- glm(churn ~ seniorcitizen+partner+dependents+multiplelines+
                techsupport+contract+streamingtv+streamingmovies+ paperlessbilling+monthlycharges+
                totalcharges,family = binomial(link = "logit"), data=df)
summary(logit1)

df <- df[df$internetservice %in% c( "DSL","Fiber optic"),]
logit2 <- glm(churn ~ seniorcitizen+partner+dependents+multiplelines+techsupport+
                contract+monthlycharges+totalcharges+onlinesecurity+onlinebackup+deviceprotection
              +streamingmovies+streamingtv, family = binomial(link = "logit"), data=df)
summary(logit2)

logit3 <- glm(churn ~ seniorcitizen+partner+dependents+techsupport
              +contract+paperlessbilling+monthlycharges+totalcharges+onlinesecurity+onlinebackup
              +deviceprotection+streamingmovies+streamingtv, family = binomial(link = "logit"), data=df) 
summary(logit3)

library(stargazer)
stargazer(logit1, logit2, logit3, title="Telcom Churn", type="text", single.row=TRUE)
AIC(logit1,logit2,logit3)
BIC(logit1, logit2, logit3)
#comparing models
logit1$coef
exp(logit1$coef) 

logit2$coef
exp(logit2$coef) 

logit3$coef
exp(logit3$coef) 

library(lmtest) 
lrtest(logit1, logit2, logit3) 

head(predict(logit1, type="link"))                         # Predicted values of y (in the scale of linear predictors)
dlogis(predict(logit1, type="link")) 

# Classification metrics using training and test datasets
set.seed(1024)
trainIndex <- sample(1:nrow(df), size=round(0.75*nrow(df)), replace=FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]
dim(train); dim(test)

logit1_c <- glm(churn ~ seniorcitizen+partner+dependents+multiplelines+
                techsupport+contract+streamingtv+streamingmovies+ paperlessbilling+monthlycharges+
                totalcharges,family = binomial(link = "logit"), data=train)
summary(logit1_c)
test_x <- test[ , c(2:20)]

predlogit <- predict(logit1_c, newdata=test_x, type="response")

predlogit <- ifelse(predlogit>0.5, 1, 0)

table(test$churn, predlogit)

logit2_c <- glm(churn ~ seniorcitizen+partner+dependents+multiplelines+techsupport+
                contract+monthlycharges+totalcharges+onlinesecurity+onlinebackup+deviceprotection
              +streamingmovies+streamingtv, family = binomial(link = "logit"), data=train)

summary(logit2_c)
test_y <- test[ , c(2:20)]

predlogit <- predict(logit1_c, newdata=test_y, type="response")

predlogit <- ifelse(predlogit>0.5, 1, 0)

table(test$churn, predlogit)

ClassificationError <- mean(predlogit != test$churn) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuraty rate

# ROC plot: TPR vs FPR
library(ROCR)
pr <- prediction(predlogit, test$churn)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)

auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc                                                       # Area Under the Curve
