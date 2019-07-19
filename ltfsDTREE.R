
setwd("C:/Users/SwaroopKM99/Documents/R/R_working_directory")

ltfs = read.csv("C:/Users/SwaroopKM99/Documents/R/R_working_directory/train_LTFS.csv", header = T,
                sep = ",")

ltfs$Employment.Type = ifelse(ltfs$Employment.Type == "Salaried" , 1, 0)

View(ltfs)
str(ltfs)
dim(ltfs)


# Create Training Data
input_ones = ltfs[which(ltfs$loan_default == 1), ]  # all 1's

input_zeros = ltfs[which(ltfs$loan_default == 0), ]  # all 0's

set.seed(100)  # for repeatability of samples

input_ones_training_rows = sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training

input_zeros_training_rows = sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's

training_ones = input_ones[input_ones_training_rows, ] 

training_zeros = input_zeros[input_zeros_training_rows, ]

trainingData = rbind(training_ones, training_zeros)  # row bind the 1's and 0's
#View(trainingData)

# Create Test Data

test_ones = input_ones[-input_ones_training_rows, ]

test_zeros = input_zeros[-input_zeros_training_rows, ]

testData = rbind(test_ones, test_zeros)  # row bind the 1's and 0's


#Model1: No Splitting

Model1=rpart(as.factor(loan_default)~. ,data= trainingData, method="class")
Model1
fancyRpartPlot(Model1)

#Confusion Matrix
Pred=predict(Model1,newdata= trainingData,type="class")
confusionMatrix(table(Pred,trainingData$loan_default))


#Model2: Splitting Criteria as information gain
Model2=rpart(loan_default ~ . ,data=trainingData,method="class",parms = list(split="information"))
Model2
fancyRpartPlot(Model2)

#Confusion Matrix
Pred=predict(Model2,newdata=trainingData,type="class")
confusionMatrix(table(Pred,trainingData$loan_default))

#Model3: Splitting Criteria as gini index
Model3=rpart(as.factor(loan_default)~.,data=trainingData,method="class",parms=list(split="gini"))
Model3
fancyRpartPlot(Model3)

#Confusion Matrix
Pred=predict(Model3,newdata=trainingData,type="class")
confusionMatrix(table(Pred,trainingData$loan_default))
