
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
#View(testData)

###############################################################################

cr = cor(trainingData)
corrplot(cr, type = "lower",  na.label = "square", na.label.col = "orange")

# Logistic Model

model.1 = glm(loan_default ~ . , data = trainingData, family = binomial(link = "logit"))
summary(model.1)

model.2 = glm(loan_default ~ disbursed_amount+ltv+branch_id+supplier_id+manufacturer_id+
                Current_pincode_ID+Age+Employment.Type+State_ID+Employee_code_ID+
                Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+PERFORM_CNS.SCORE+
                PRI.ACTIVE.ACCTS+PRI.OVERDUE.ACCTS+PRI.CURRENT.BALANCE+
                DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+AVERAGE.ACCT.AGE.months+
                CREDIT.HISTORY.LENGTH.months+NO.OF_INQUIRIES, data = trainingData, family = binomial(link = "logit"))
summary(model.2)

model.3 = glm(loan_default ~ disbursed_amount+ltv+branch_id+supplier_id+manufacturer_id+
                Age+Employment.Type+State_ID+Employee_code_ID+
                Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+PERFORM_CNS.SCORE+
                PRI.ACTIVE.ACCTS+PRI.OVERDUE.ACCTS+PRI.CURRENT.BALANCE+
                DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+AVERAGE.ACCT.AGE.months+
                CREDIT.HISTORY.LENGTH.months+NO.OF_INQUIRIES, data = trainingData, family = binomial(link = "logit"))
summary(model.3)

model.4 = glm(loan_default ~ disbursed_amount+ltv+branch_id+supplier_id+manufacturer_id+
                Age+Employment.Type+State_ID+Employee_code_ID+
                Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+PERFORM_CNS.SCORE+
                PRI.OVERDUE.ACCTS+PRI.CURRENT.BALANCE+
                DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+AVERAGE.ACCT.AGE.months+
                CREDIT.HISTORY.LENGTH.months+NO.OF_INQUIRIES, data = trainingData, family = binomial(link = "logit"))
summary(model.4)

model.5 = glm(loan_default ~ disbursed_amount+ltv+branch_id+supplier_id+manufacturer_id+
                Age+Employment.Type+State_ID+Employee_code_ID+
                Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+PERFORM_CNS.SCORE+
                PRI.NO.OF.ACCTS+PRI.OVERDUE.ACCTS+PRI.CURRENT.BALANCE+
                DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+AVERAGE.ACCT.AGE.months+
                CREDIT.HISTORY.LENGTH.months+NO.OF_INQUIRIES, data = trainingData, family = binomial(link = "logit"))
summary(model.5)

model.6 = glm(loan_default ~ disbursed_amount+ltv+branch_id+supplier_id+manufacturer_id+
                Age+Employment.Type+State_ID+Employee_code_ID+
                Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+PERFORM_CNS.SCORE+
                PRI.NO.OF.ACCTS+PRI.OVERDUE.ACCTS+PRI.CURRENT.BALANCE+
                AVERAGE.ACCT.AGE.months+
                CREDIT.HISTORY.LENGTH.months+NO.OF_INQUIRIES, data = trainingData, family = binomial(link = "logit"))
summary(model.6)

model.7 = glm(loan_default ~ disbursed_amount+ltv+branch_id+supplier_id+manufacturer_id+
                Age+Employment.Type+State_ID+Employee_code_ID+
                Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+PERFORM_CNS.SCORE+
                PRI.NO.OF.ACCTS+PRI.OVERDUE.ACCTS+PRI.CURRENT.BALANCE+
                CREDIT.HISTORY.LENGTH.months+NO.OF_INQUIRIES, data = trainingData, family = binomial(link = "logit"))
summary(model.7)

model.8 = glm(loan_default ~ disbursed_amount+ltv+branch_id+supplier_id+manufacturer_id+
                Age+Employment.Type+State_ID+Employee_code_ID+
                Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+PERFORM_CNS.SCORE+
                PRI.NO.OF.ACCTS+PRI.OVERDUE.ACCTS+PRI.CURRENT.BALANCE+
                +NO.OF_INQUIRIES, data = trainingData, family = binomial(link = "logit"))
summary(model.8)


#####

model.9 = glm(loan_default ~ disbursed_amount+ltv+branch_id+supplier_id+manufacturer_id+
                Current_pincode_ID+Age+Employment.Type+State_ID+Employee_code_ID+
                Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+
                NEW.ACCTS.IN.LAST.SIX.MONTHS+PRI.CURRENT.BALANCE+
                DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+AVERAGE.ACCT.AGE.months+
                CREDIT.HISTORY.LENGTH.months+NO.OF_INQUIRIES, data = trainingData, family = binomial(link = "logit"))
summary(model.9)


vif(model.2)

res = predict(model.2, trainingData, type = "response")
head(res)
head(trainingData)
table(Actualvalue=trainingData$loan_default, Predictedvalue=res>0.5)

ROCRpred = prediction(res, trainingData$loan_default)
ROCRpref = performance(ROCRpred, "tpr", "fpr")

plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.2,by=0.2))

# Testing Data

res1 = predict(model.2, testData, type = "response")
table(Actualvalue=testData$loan_default, Predictedvalue=res1>0.5)
ROCRpred = prediction(res1, testData$loan_default)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.2,by=0.2))

# TESTING DATA PREDICTION

ltfs_test = read.csv("C:/Users/SwaroopKM99/Documents/R/R_working_directory/test_LTFS.csv", header = T,
                     sep = ",")

ltfs_test$Employment.Type = ifelse(ltfs_test$Employment.Type == "Salaried" , 1, 0)

str(ltfs_test)
dim(ltfs_test)


prediction = predict(model.2, ltfs_test, type = "response")
Loan_Status = ifelse( prediction > .5, 1, 0)
output = cbind(ltfs_test, Loan_Status)

View(output)



write.csv(output, file = "Solution.csv", row.names = FALSE)






































