banking = read.csv("C:/Users/dickm/Documents/Projects/ML/Source/UPMC/AWS/bank/bank.csv")

library(caTools)
set.seed(144)
spl = sample.split(banking$y, 0.75)
train = subset(banking, spl == TRUE)
test = subset(banking, spl == FALSE)

modelLM = glm(y~., data=train, family=binomial)

#What's important in the model
summary(modelLM)

predLM = predict(modelLM, type="response", newdata=test)

table(test$y,predLM > 0.5)

#install.packages("ROCR")
library("ROCR")

ROCRpred = prediction(predLM, test$y)
as.numeric(performance(ROCRpred, "auc")@y.values)


# Using more data

bankingFull = read.csv("C:/Users/dickm/Documents/Projects/ML/Source/UPMC/AWS/bank/bank-full.csv")
str(bankingFull)

