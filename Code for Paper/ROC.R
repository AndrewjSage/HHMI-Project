library(party)
library(pROC)
library(caret)
library(ggplot2)
library(mice)
################################################################################
#ROC curves for ISU retention


set.seed(04082017)
setwd("~/Box Sync/Iowa State/Engage Analysis/Paper/Data and R Code/RData Files")
load("STEMfiles.Rdata")
STEM <- STEM1[(STEM1$dataset_term>2013) & (STEM1$dataset_term<2016),]
STEM$Class=as.numeric(STEM$Class==2)

exclude <- c("anonId", "majorCurrStart.1", "Sem3begSTEM", "EnrS3", "dataset_term", "STEMCredits")
Include <- c("MajCat", "Class")
HSAcademics <- c("hsGpa", "hsRank", "HSMTH", "HSSCI")
Demographics <- c("Ethnicity", "USctzn", "IARes", "sexCd", "MW_ParentEd")
Activities <- c("Sport", "greek")
ISUAwards <- c("Carver", "Hixson", "MVP")
MW_AcademicSkills <- c("MW_AcademicSkills")
MW_SocialIntegration <- c("MW_SocialIntegration")
MW_MajSatisfaction <- c("MW_ChangeMaj")
MW_ISUSatisfaction <- c("MW_ISUSatisfaction")
MW_MathSciSelfEfficacy <- c("MW_MathSciSelfEfficacy")
MW_FinancialConcerns <- c("MWFinancialConcerns")
Tests <- c("ALEKS_goalAfter", "actCmpst", "satVrbl", "satMath", "actEngl", "actMath", "actRead")
Sem1Classes <- c("Phys", "Chem", "Biol", "Math14", "Calc1", "Calc2", "Psych131", "SCICourses")
MidtermGrades <- c("STEMMidterm", "MidtermPoints")
ACTInterest <- c("ACTINVSTEMMaj", "MajorSurenessAdj")  
LCommunity <- c("LC_member")


#Get ROC Curves
DATA14 <- STEM[STEM$dataset_term==2014,]
DATA15 <- STEM[STEM$dataset_term==2015,]
DATA14 <- subset(DATA14, select=-c(anonId, majorCurrStart.1, Sem3begSTEM, EnrS3, dataset_term, PlannedEC, satVrbl, satMath,SCICourses))
DATA15 <- subset(DATA15, select=-c(anonId, majorCurrStart.1, Sem3begSTEM, EnrS3, dataset_term, PlannedEC, satVrbl, satMath,SCICourses))
Academic <- c(HSAcademics, Tests, MidtermGrades, Sem1Classes, Include)
NonAcademic <- c(Activities, MW_AcademicSkills, MW_SocialIntegration, MW_MajSatisfaction, MW_ISUSatisfaction, MW_MathSciSelfEfficacy, MW_FinancialConcerns, ACTInterest,LCommunity, Demographics,ISUAwards, Include)

DATA14ac <- DATA14[, names(DATA14)%in%Academic]
DATA15ac <- DATA15[, names(DATA15)%in%Academic]
DATA14nac <- DATA14[, names(DATA14)%in%NonAcademic]
DATA15nac <- DATA15[, names(DATA15)%in%NonAcademic]



########Logistic regression model for comparison

#Impute missing values using Multivariate Imputation using Chained Equations

#Don't include response variable in imputation

set.seed(02062016)
DATA14imp <- DATA14[,-c(19,25)]
DATA14imp <- mice(DATA14imp, m = 1, method = vector("character", length = ncol(DATA14imp)),
          predictorMatrix = (1 - diag(1, ncol(DATA14imp))),
          visitSequence = (1:ncol(DATA14imp))[apply(is.na(DATA14imp), 2, any)],
          form = vector("character", length = ncol(DATA14imp)),
          post = vector("character", length = ncol(DATA14imp)), defaultMethod = c("pmm",
                                                                             "logreg", "polyreg", "polr"), maxit = 5, diagnostics = TRUE,
          printFlag = TRUE, seed = NA, imputationMethod = NULL,
          defaultImputationMethod = NULL, data.init = NULL)
DATA14imp <- complete(DATA14imp,1)
DATA14imp$Class <- DATA14$Class

DATA15imp <- DATA15[,-c(19,25)]
DATA15imp <- mice(DATA15imp, m = 1, method = vector("character", length = ncol(DATA15imp)),
               predictorMatrix = (1 - diag(1, ncol(DATA15imp))),
               visitSequence = (1:ncol(DATA15imp))[apply(is.na(DATA15imp), 2, any)],
               form = vector("character", length = ncol(DATA15imp)),
               post = vector("character", length = ncol(DATA15imp)), defaultMethod = c("pmm",
                                                                                    "logreg", "polyreg", "polr"), maxit = 5, diagnostics = TRUE,
               printFlag = TRUE, seed = NA, imputationMethod = NULL,
               defaultImputationMethod = NULL, data.init = NULL)
DATA15imp <- complete(DATA15imp,1)
DATA15imp$Class <- DATA15$Class


#Fit model and get auc
M <- glm(factor(Class) ~.,family=binomial(link='logit'),data=DATA14imp)
LRPred <- predict(M, newdata=DATA15imp, type="response")
auc(as.numeric(as.character(DATA15$Class)), LRPred)


#All factors
set.seed(09182017)
CF <- cforest(as.factor(Class)~., data=DATA14, controls=cforest_unbiased(ntree=1000, minsplit=75))
CFPred <- unlist(predict(CF, newdata=DATA15, type="prob"))[seq(2,2*nrow(DATA15),2)]
CFPredISU <- CFPred
auc(as.numeric(as.character(DATA15$Class)), CFPred)

#Academic only
set.seed(09182017)
CF2 <- cforest(as.factor(Class)~., data=DATA14ac, controls=cforest_unbiased(ntree=1000, minsplit=75))
CFPred2 <- unlist(predict(CF2, newdata=DATA15ac, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred2)

#non academic only
set.seed(09182017)
CF3 <- cforest(as.factor(Class)~., data=DATA14nac, controls=cforest_unbiased(ntree=1000, minsplit=75))
CFPred3 <- unlist(predict(CF3, newdata=DATA15nac, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred3)



#Function to produce ROC curve
ROC <- function (x,truth,add=F)
{
  y <- truth[order(-x)]     #y contains 0-1 outcome arranged in order from most likely to leave to least likely                   
  ones <- sum(y)           #total number that left
  zeros <- sum(1-y)        #total number that stayed
  sensitivity <- cumsum(y)/ones       #vector containing prop. of students leaving in group of size n most likely to leave (true positives)
  specificity <- 1-cumsum(1-y)/zeros  #vector containing prop. of students staying in group of size n most likely to leave (false positives)
  df <- data.frame(sensitivity, specificity)
  return(df)
}

Truth <- as.numeric(as.character(DATA15$Class))


dev.off()
#To look at ROC curves 
AllROC <- ROC(CFPred, Truth)
plot(1-AllROC$specificity,AllROC$sensitivity,type="l", col="blue", ylab="True Positive Rate", xlab="False Positive Rate", pch=16)
AcademicROC <- ROC(CFPred2, Truth)
lines(1-AcademicROC$specificity,AcademicROC$sensitivity,type="l", col="red", ylab="True Positive Rate", xlab="False Positive Rate")
NonAcademicROC <- ROC(CFPred3, Truth)
lines(1-NonAcademicROC$specificity,NonAcademicROC$sensitivity,type="l", col="green", ylab="True Positive Rate", xlab="False Positive Rate")
legend("bottomright", c("Full Model", "Academic Only", "Nonacademic Only"), lty=c(1,1,1), col=c("blue", "red", "green"), cex=.75)

################################################################################
#ROC curves for STEM retention

set.seed(04082017)
setwd("~/Box Sync/Iowa State/Engage Analysis/Paper/Data and R Code/RData Files")
load("STEMfiles.Rdata")

exclude <- c("anonId", "majorCurrStart.1", "Sem3begSTEM", "EnrS3", "dataset_term", "STEMCredits")
Include <- c("MajCat", "Class")
HSAcademics <- c("hsGpa", "hsRank", "HSMTH", "HSSCI")
Demographics <- c("Ethnicity", "USctzn", "IARes", "sexCd", "MW_ParentEd")
Activities <- c("Sport", "greek")
ISUAwards <- c("Carver", "Hixson", "MVP")
MW_AcademicSkills <- c("MW_AcademicSkills")
MW_SocialIntegration <- c("MW_SocialIntegration")
MW_MajSatisfaction <- c("MW_ChangeMaj")
MW_ISUSatisfaction <- c("MW_ISUSatisfaction")
MW_MathSciSelfEfficacy <- c("MW_MathSciSelfEfficacy")
MW_FinancialConcerns <- c("MWFinancialConcerns")
Tests <- c("ALEKS_goalAfter", "actCmpst", "satVrbl", "satMath", "actEngl", "actMath", "actRead")
Sem1Classes <- c("Phys", "Chem", "Biol", "Math14", "Calc1", "Calc2", "Psych131", "SCICourses")
MidtermGrades <- c("STEMMidterm", "MidtermPoints")
ACTInterest <- c("ACTINVSTEMMaj", "MajorSurenessAdj")  
LCommunity <- c("LC_member")

#Get ROC Curves
DATA14 <- STEM[STEM$dataset_term==2014,]
DATA15 <- STEM[STEM$dataset_term==2015,]
DATA14 <- subset(DATA14, select=-c(anonId, majorCurrStart.1, Sem3begSTEM, EnrS3, dataset_term, PlannedEC, satVrbl, satMath,SCICourses))
DATA15 <- subset(DATA15, select=-c(anonId, majorCurrStart.1, Sem3begSTEM, EnrS3, dataset_term, PlannedEC, satVrbl, satMath,SCICourses))
Academic <- c(HSAcademics, Tests, MidtermGrades, Sem1Classes, Include)
NonAcademic <- c(Activities, MW_AcademicSkills, MW_SocialIntegration, MW_MajSatisfaction, MW_ISUSatisfaction, MW_MathSciSelfEfficacy, MW_FinancialConcerns, ACTInterest,LCommunity, Demographics,ISUAwards, Include)
NoMaj <- unique(c(Academic, NonAcademic))

DATA14ac <- DATA14[, names(DATA14)%in%Academic]
DATA15ac <- DATA15[, names(DATA15)%in%Academic]
DATA14nac <- DATA14[, names(DATA14)%in%NonAcademic]
DATA15nac <- DATA15[, names(DATA15)%in%NonAcademic]

#We've already imputed missing values using all students. Don't do it again. 
#at this point, response Class is for STEM retention. 
#Get rid of those who left ISU (Class==1 in DATAimp) then replace Class variable with STEM retention from DATA14
DATA14imp <- DATA14imp[DATA14imp$Class==0,]
DATA15imp <- DATA15imp[DATA15imp$Class==0,]
DATA14imp$Class <- DATA14$Class  #now response variable is STEM retention, not ISU retention
DATA15imp$Class <- DATA15$Class  #now response variable is STEM retention, not ISU retention


#Fit model and get auc
M <- glm(factor(Class) ~.,family=binomial(link='logit'),data=DATA14imp)
LRPred <- predict(M, newdata=DATA15imp, type="response")
LRPred
auc(as.numeric(as.character(DATA15$Class)), LRPred)


#AUC for all factors
set.seed(09182017)
CF=cforest(as.factor(Class)~., data=DATA14, controls=cforest_unbiased(ntree=1000, minsplit=75))
CFPred=unlist(predict(CF, newdata=DATA15, type="prob"))[seq(2,2*nrow(DATA15),2)]
CFPredSTEM <- CFPred
auc(as.numeric(as.character(DATA15$Class)), CFPred)

#Only academic factors
set.seed(09182017)
CF2=cforest(as.factor(Class)~., data=DATA14ac, controls=cforest_unbiased(ntree=1000, minsplit=75))
CFPred2=unlist(predict(CF2, newdata=DATA15ac, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred2)

#Only nonacademic factors
set.seed(09182017)
CF3=cforest(as.factor(Class)~., data=DATA14nac, controls=cforest_unbiased(ntree=1000, minsplit=75))
CFPred3=unlist(predict(CF3, newdata=DATA15nac, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred3)


#Function to produce ROC curve
ROC=function (x,truth,add=F)
{
  y=truth[order(-x)]     #y contains 0-1 outcome arranged in order from most likely to leave to least likely                   
  ones=sum(y)           #total number that left
  zeros=sum(1-y)        #total number that stayed
  sensitivity=cumsum(y)/ones       #vector containing prop. of students leaving in group of size n most likely to leave (true positives)
  specificity=1-cumsum(1-y)/zeros  #vector containing prop. of students staying in group of size n most likely to leave (false positives)
  df=data.frame(sensitivity, specificity)
  return(df)
}

Truth=as.numeric(as.character(DATA15$Class))


dev.off()
#To look at ROC curves 
AllROC=ROC(CFPred, Truth)
plot(1-AllROC$specificity,AllROC$sensitivity,type="l", col="blue", ylab="True Positive Rate", xlab="False Positive Rate", pch=16)
AcademicROC=ROC(CFPred2, Truth)
lines(1-AcademicROC$specificity,AcademicROC$sensitivity,type="l",  col="red", ylab="True Positive Rate", xlab="False Positive Rate", main="LR ROC Curves")
NonAcademicROC=ROC(CFPred3, Truth)
lines(1-NonAcademicROC$specificity,NonAcademicROC$sensitivity,type="l", col="green", ylab="True Positive Rate", xlab="False Positive Rate")
legend("bottomright", c("Combined", "Cognitive Only", "Noncognitive Only"), lty=c(1,1,1), col=c("blue", "red", "green"), cex=.75)

#Plot with different line types

dev.off()
#To look at ROC curves 
AllROC=ROC(CFPred, Truth)
plot(1-AllROC$specificity,AllROC$sensitivity,type="l", col="blue", ylab="True Positive Rate", xlab="False Positive Rate", pch=16)
AcademicROC=ROC(CFPred2, Truth)
lines(1-AcademicROC$specificity,AcademicROC$sensitivity,type="l", lty=2,  col="red", ylab="True Positive Rate", xlab="False Positive Rate", main="LR ROC Curves")
NonAcademicROC=ROC(CFPred3, Truth)
lines(1-NonAcademicROC$specificity,NonAcademicROC$sensitivity,type="l", lty=3,col="green4", ylab="True Positive Rate", xlab="False Positive Rate")
legend("bottomright", c("Combined", "Cognitive Only", "Noncognitive Only"), lty=c(1,2,3), col=c("blue", "red", "green4"), cex=.75)
