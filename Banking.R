library(caTools)
#install.packages("ROCR")
library("ROCR")

start.time = proc.time()
# read data
banking = read.csv("https://s3.amazonaws.com/aws.banking.sample/bank.csv")

# split data
set.seed(144)
spl = sample.split(banking$y, 0.70)
train = subset(banking, spl == TRUE)
test = subset(banking, spl == FALSE)

# Train model
modelLM = glm(y~., data=train, family=binomial)


# Evaluate Model
predLM = predict(modelLM, type="response", newdata=test)

#Threshold 0.5
table(test$y,predLM > 0.5)

#Threshold 0.88
table(test$y,predLM > 0.88)

ROCRpred = prediction(predLM, test$y)
as.numeric(performance(ROCRpred, "auc")@y.values)
proc.time() - start.time

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot AUC graph
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Inspect model
summary(modelLM)



