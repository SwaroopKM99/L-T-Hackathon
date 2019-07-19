
setwd("C:/Users/SwaroopKM99/Documents/R/R_working_directory")

ltfs = read.csv("C:/Users/SwaroopKM99/Documents/R/R_working_directory/train_LTFS.csv", header = T,
                 sep = ",")
ltfs_test = read.csv("C:/Users/SwaroopKM99/Documents/R/R_working_directory/test_LTFS.csv", header = T,
                sep = ",")

str(ltfs)
summary(ltfs)
dim(ltfs)
View(ltfs)

# Treating the missing value in employment.type variable

getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result1 = getmode(ltfs$Employment.Type)
print(result1)
ltfs$Employment.Type[ltfs$Employment.Type == "" ] = result1

# correlation matrix

correlation = subset(ltfs, select = -c(Date.of.Birth, Employment.Type, DisbursalDate, PERFORM_CNS.SCORE.DESCRIPTION))
cr = cor(correlation, method = "pearson")
corrplot(cr, type = "lower",  na.label = "square", na.label.col = "orange")

ltfs$Date.of.Birth = NULL
ltfs$DisbursalDate = NULL

# model 

ltfs_model = glm(loan_default ~ ., data = ltfs, family = "binomial")
summary(ltfs_model)

model1 = glm(loan_default ~ UniqueID+ disbursed_amount+asset_cost+ltv+branch_id+
               supplier_id+manufacturer_id+Current_pincode_ID+State_ID+Employee_code_ID+
               Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+PERFORM_CNS.SCORE+
               PRI.NO.OF.ACCTS+PRI.ACTIVE.ACCTS+PRI.OVERDUE.ACCTS+PRI.CURRENT.BALANCE+
               SEC.NO.OF.ACCTS+NEW.ACCTS.IN.LAST.SIX.MONTHS+DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+
               AVERAGE.ACCT.AGE.months+CREDIT.HISTORY.LENGTH.months+NO.OF_INQUIRIES, 
               data = ltfs, family = binomial(link = "logit"))
summary(model1)

model2 = glm(loan_default ~ UniqueID+asset_cost+ltv+branch_id+
               supplier_id+manufacturer_id+Current_pincode_ID+State_ID+Employee_code_ID+
               Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+PERFORM_CNS.SCORE+
               PRI.DISBURSED.AMOUNT+PRI.ACTIVE.ACCTS+PRI.OVERDUE.ACCTS+PRI.CURRENT.BALANCE+
               SEC.ACTIVE.ACCTS+DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+SEC.CURRENT.BALANCE+
               AVERAGE.ACCT.AGE.months+NO.OF_INQUIRIES, data = ltfs, family = binomial(link = "logit"))
summary(model2)

model3 = glm(loan_default ~ UniqueID+asset_cost+ltv+branch_id+
               supplier_id+manufacturer_id+Current_pincode_ID+State_ID+Employee_code_ID+
               Aadhar_flag+PAN_flag+Driving_flag+Passport_flag+PERFORM_CNS.SCORE+
               PRI.ACTIVE.ACCTS+PRI.OVERDUE.ACCTS+PRI.CURRENT.BALANCE+Employment.Type+
               DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+SEC.CURRENT.BALANCE+CREDIT.HISTORY.LENGTH.months+
               NO.OF_INQUIRIES+PERFORM_CNS.SCORE.DESCRIPTION+AVERAGE.ACCT.AGE.months, 
               data = ltfs, family = binomial(link = "logit"))
summary(model3)


Dummy = dummyVars("~.",data = ltfs)
Dummy_data1 = data.frame((predict(Dummy,ltfs)))
str(Dummy_data1)

write.csv(Dummy_data1, "Dummy.csv")

model10 = glm(loan_default ~ . , data = Dummy_data1, family = binomial)
summary(model10)


model100 = glm(loan_default ~ UniqueID+asset_cost+ltv+branch_id+supplier_id+manufacturer_id+
          Current_pincode_ID+State_ID+Employee_code_ID+Aadhar_flag+PAN_flag+Driving_flag+
          Passport_flag+PERFORM_CNS.SCORE+PRI.NO.OF.ACCTS+PRI.ACTIVE.ACCTS+PRI.OVERDUE.ACCTS+
          PRI.CURRENT.BALANCE+DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+CREDIT.HISTORY.LENGTH.months+
          NO.OF_INQUIRIES+AVERAGE.ACCT.AGE.months+Employment.Type.Salaried+PERFORM_CNS.SCORE.DESCRIPTION.A.Very.Low.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.B.Very.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.C.Very.Low.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.D.Very.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.E.Low.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.F.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.G.Low.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.H.Medium.Risk+PERFORM_CNS.SCORE.DESCRIPTION.I.Medium.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.J.High.Risk+ PERFORM_CNS.SCORE.DESCRIPTION.K.High.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.L.Very.High.Risk+PERFORM_CNS.SCORE.DESCRIPTION.M.Very.High.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.No.Bureau.History.Available+PERFORM_CNS.SCORE.DESCRIPTION.Not.Scored..No.Activity.seen.on.the.customer..Inactive.+
          PERFORM_CNS.SCORE.DESCRIPTION.Not.Scored..No.Updates.available.in.last.36.months+
          PERFORM_CNS.SCORE.DESCRIPTION.Not.Scored..Not.Enough.Info.available.on.the.customer+
          PERFORM_CNS.SCORE.DESCRIPTION.Not.Scored..Only.a.Guarantor, data = Dummy_data1, family = binomial)
summary(model100)

vif(model100)


model101 = glm(loan_default ~ UniqueID+asset_cost+ltv+branch_id+supplier_id+manufacturer_id+
                 Current_pincode_ID+State_ID+Employee_code_ID+Aadhar_flag+PAN_flag+Driving_flag+
                 Passport_flag+PERFORM_CNS.SCORE+PRI.NO.OF.ACCTS+PRI.ACTIVE.ACCTS+PRI.OVERDUE.ACCTS+
                 PRI.CURRENT.BALANCE+DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+CREDIT.HISTORY.LENGTH.months+
                 NO.OF_INQUIRIES+AVERAGE.ACCT.AGE.months+Employment.Type.Salaried+PERFORM_CNS.SCORE.DESCRIPTION.A.Very.Low.Risk+
                 PERFORM_CNS.SCORE.DESCRIPTION.B.Very.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.C.Very.Low.Risk+
                 PERFORM_CNS.SCORE.DESCRIPTION.D.Very.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.E.Low.Risk+
                 PERFORM_CNS.SCORE.DESCRIPTION.F.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.G.Low.Risk+
                 PERFORM_CNS.SCORE.DESCRIPTION.H.Medium.Risk+PERFORM_CNS.SCORE.DESCRIPTION.I.Medium.Risk+
                 PERFORM_CNS.SCORE.DESCRIPTION.J.High.Risk+ PERFORM_CNS.SCORE.DESCRIPTION.K.High.Risk+
                 PERFORM_CNS.SCORE.DESCRIPTION.L.Very.High.Risk+PERFORM_CNS.SCORE.DESCRIPTION.M.Very.High.Risk+
                 PERFORM_CNS.SCORE.DESCRIPTION.No.Bureau.History.Available, 
                 data = Dummy_data1, family = binomial)
summary(model101)


model102 = glm(loan_default ~ UniqueID+asset_cost+ltv+branch_id+supplier_id+manufacturer_id+
        Current_pincode_ID+State_ID+Employee_code_ID+Aadhar_flag+PAN_flag+Driving_flag+
        Passport_flag+PERFORM_CNS.SCORE+PRI.NO.OF.ACCTS+PRI.ACTIVE.ACCTS+PRI.OVERDUE.ACCTS+
        PRI.CURRENT.BALANCE+DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+CREDIT.HISTORY.LENGTH.months+
        NO.OF_INQUIRIES+AVERAGE.ACCT.AGE.months+Employment.Type.Salaried+
        PERFORM_CNS.SCORE.DESCRIPTION.A.Very.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.B.Very.Low.Risk+
        PERFORM_CNS.SCORE.DESCRIPTION.C.Very.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.D.Very.Low.Risk+
        PERFORM_CNS.SCORE.DESCRIPTION.E.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.F.Low.Risk+
        PERFORM_CNS.SCORE.DESCRIPTION.G.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.H.Medium.Risk+
        PERFORM_CNS.SCORE.DESCRIPTION.I.Medium.Risk+PERFORM_CNS.SCORE.DESCRIPTION.J.High.Risk+
        PERFORM_CNS.SCORE.DESCRIPTION.K.High.Risk+PERFORM_CNS.SCORE.DESCRIPTION.L.Very.High.Risk+
        PERFORM_CNS.SCORE.DESCRIPTION.M.Very.High.Risk, data = Dummy_data1, family = binomial)
summary(model102)


vif(model102)

model200 = glm(loan_default ~ disbursed_amount+ltv+branch_id+supplier_id+manufacturer_id+
          Employment.Type.Salaried+Employee_code_ID+Aadhar_flag+Passport_flag+PERFORM_CNS.SCORE.DESCRIPTION.A.Very.Low.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.B.Very.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.C.Very.Low.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.D.Very.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.E.Low.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.F.Low.Risk+PERFORM_CNS.SCORE.DESCRIPTION.G.Low.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.I.Medium.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.L.Very.High.Risk+
          PERFORM_CNS.SCORE.DESCRIPTION.Not.Scored..No.Activity.seen.on.the.customer..Inactive.+
          PERFORM_CNS.SCORE.DESCRIPTION.Not.Scored..No.Updates.available.in.last.36.months+
          PERFORM_CNS.SCORE.DESCRIPTION.Not.Scored..Not.Enough.Info.available.on.the.customer+
          PRI.DISBURSED.AMOUNT+SEC.CURRENT.BALANCE+PRIMARY.INSTAL.AMT+
          NO.OF_INQUIRIES, data = Dummy_data1, family = binomial)

summary(model200)

vif(model200)

res = predict(model200, Dummy_data1, type = "response")
head(res)
head(Dummy_data1)
table(Actualvalue=Dummy_data1$loan_default, Predictedvalue=res>0.5)

ROCRpred = prediction(res, Dummy_data1$loan_default)
ROCRpref = performance(ROCRpred, "tpr", "fpr")

plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.2,by=0.2))






