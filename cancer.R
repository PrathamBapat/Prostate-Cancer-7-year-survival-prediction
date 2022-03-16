setwd("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Cases/Prostate Cancer Analysis/participant_files/participant_files")
pca_test <- read.csv(file="(name)_score.csv", header=TRUE, sep=",")
View(pca_test)
str(pca_test)
str(pca_test$age)

pca_test$age_cat <- NA
pca_test$survival_7_years <- NULL
pca_test$age_cat <- pca_test$age

pca_test$age_cat[pca_test$age<=60] <- 1
pca_test$age_cat[pca_test$age>60 & pca_test$age<=70] <- 2
pca_test$age_cat[pca_test$age>70 & pca_test$age<=80] <- 3
pca_test$age_cat[pca_test$age>80 & pca_test$age<=90] <- 4
pca_test$age_cat[pca_test$age>90] <- 5

str(pca_test$age_cat)
pca_test$age_cat <- as.factor(pca_test$age_cat)
table(pca_test$age_cat)

t <-table(pca_test$age_cat)
plot(pca_test$age_cat) # Oops 
barplot(t, main = "Bar Plot", xlab = "predictions", ylab = "Frequency")
ptab<-prop.table(t)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Bar Plot", xlab = "predictions", ylab = "Proportion")
barplot(ptab, main = "Bar Plot", xlab = "predictions", ylab = "Proportion", col="steelblue")
barplot(ptab, main = "Bar Plot", xlab = "predictions", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,45))
box()




#Creating BMI Variable
BMI = function(height,weight){
  return(0.45455*weight/(.0254*height)^2)}

pca_test$BMI <- BMI(pca_test$height,pca_test$weight)


pca_test$obese <- NA


pca_test$obese[pca_test$BMI<=30] <- 0
pca_test$obese[pca_test$BMI>30] <- 1

str(pca_test$obese)

pca_test$obese <- as.factor(pca_test$obese)
table(pca_test$obese)
str(pca_test$stage)
str(pca_test$gleason_score)
str(pca_test$psa_diagnosis)

# pca_test$high_risk_ind <- NA
# pca_test$high_risk_ind <- ifelse(pca_test$stage == "III" | pca_test$stage == "IV" | pca_test$gleason_score>=7 | pca_test$psa_diagnosis >=10 , 1 , 0)
# str(pca_test$high_risk_ind)
# pca_test$high_risk_ind <- as.factor(pca_test$high_risk_ind)
# 
# table(pca_test$high_risk_ind)

pca_test$high_risk_indicator <- NA
pca_test$high_risk_indicator <- ifelse((pca_test$stage == "III" | pca_test$stage == "IV") & pca_test$gleason_score>=7 & pca_test$psa_diagnosis >=10 , 1 , 0)
table(pca_test$high_risk_indicator)



pca_test <- as.data.frame(pca_test)
cols <- c("rd_thrpy", "h_thrpy", "chm_thrpy", "cry_thrpy","brch_thrpy","rad_rem","multi_thrpy","survival_1_year","race","previous_cancer",
          "smoker","high_risk_indicator")

pca_test[,cols] <-  data.frame(apply(pca_test[cols],2, as.factor))
class(pca_test)

str(pca_test)

# Write CSV in R
write.csv(pca_test, file = "test.csv",row.names=FALSE)

#########################################################
#Logistic Regression
data(pca_test)
View(pca_test)
str(pca_test)
write.csv(pca_test, file = "training.csv",row.names=FALSE)


## 75% of the sample size
smp_size <- floor(0.75 * nrow(pca_test))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(pca_test)), size = smp_size)

train <- pca_test[train_ind, ]
test <- pca_test[-train_ind, ]

View(train)
View(test)

summary(train)
#Training data With All Input Variables
glm.fit.train <- glm(survival_7_years ~age_cat+race+first_degree_history+family_history+t_score+n_score
                     +m_score+high_risk_indicator+obese+smoker+rd_thrpy+h_thrpy+chm_thrpy+cry_thrpy+brch_thrpy
                     +rad_rem+multi_thrpy+tumor_1_year, data=pca_test, family="binomial")
summary(glm.fit.train)


glm.predict.train <- predict(glm.fit.train, type="response", newdata=train,na.action = na.pass)
glm.classify.train <- ifelse(glm.predict.train >.5, "Yes", "No")
glm.classify.train <-as.factor(glm.classify.traindata)

Predicted <- glm.classify.train
Predicted <- factor(Predicted,levels(Predicted)[c(2,1)])
Actual  <- train$Direction
Actual <- factor(Actual,levels(Actual)[c(2,1)])

confmat.train <- table(Predicted,Actual)
confmat.train

library(caret)
confusionMatrix(confmat.traindata)

recallAllVarsTrain <- recall(Predicted, Actual, relevant = levels(Actual)[1])
prescisionAllVarsTrain <-precision(Predicted, Actual, relevant = levels(Actual)[1])
FscoreAllVarsTrain <-F_meas(Predicted, Actual, relevant = levels(Actual)[1])

trainerror <- mean(glm.classify.traindata != train$Direction)
print(paste('Train Accuracy: ',1-trainerror))
trainaccAllPredVars <-1-trainerror
trainaccAllPredVars

glm.predict.test <- predict(glm.fit.train, type="response", newdata=test)
glm.classify.testdata <- ifelse(glm.predict.test >.5, "Up", "Down")
glm.classify.testdata <-as.factor(glm.classify.testdata)


Predicted <- glm.classify.testdata
Predicted <- factor(Predicted,levels(Predicted)[c(2,1)])
Actual  <- test$Direction
Actual <- factor(Actual,levels(Actual)[c(2,1)])

confmat.testdata <- table(Predicted,Actual)
confmat.testdata

confusionMatrix(confmat.testdata)

#Test data With All Input Variables
recallAllVarsTest <- recall(Predicted, Actual, relevant = levels(Actual)[1])
prescisionAllVarsTest <-precision(Predicted, Actual, relevant = levels(Actual)[1])
FscoreAllVarsTest <-F_meas(Predicted, Actual, relevant = levels(Actual)[1])

testerror <- mean(glm.classify.testdata != test$Direction)
print(paste('Test Accuracy: ',1-testerror))

testaccAllPredVars <-1-testerror
testaccAllPredVars

corrplot(cormat, method="shade", addCoef.col="black")

tumor <- pca_test_tumor[,c(17,18,19)] # Making a numeric dataset
View(tumor)
cormat <- cor(tumor) # Correlation matrix
round(cormat, 2) # Rounding off to two decimal places
library(corrplot)
corrplot(cormat, method="shade", addCoef.col="black")

str(pca_test)


pca_test_tumor <- pca_test
pca_test_tumor <-na.omit(pca_test_tumor) # to remove NAs
View(pca_test_tumor)
str(pca_test_tumor)

pca_test_pca_testlevel <- pca_test
str(pca_test_pca_testlevel)

pca_testlevel <- pca_test_pca_testlevel[,c(20,21,22)] # Making a numeric dataset
View(pca_testlevel)
pca_testlevel <-na.omit(pca_testlevel) # to remove NAs
View(pca_testlevel)
str(pca_testlevel)
cormat <- cor(pca_testlevel) # Correlation matrix
round(cormat, 2) # Rounding off to two decimal places
library(corrplot)
corrplot(cormat, method="shade", addCoef.col="black")