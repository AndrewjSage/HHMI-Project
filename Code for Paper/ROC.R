library(party)
library(pROC)
library(caret)
library(ggplot2)
################################################################################

################################################################################
#ROC curves for STEM retention

set.seed(04082017)
setwd("~/Box Sync/Iowa State/Engage Analysis/Current Code/Summer 2017")
load("STEMfiles.Rdata")

exclude=c("anonId", "majorCurrStart.1", "Sem3begSTEM", "EnrS3", "dataset_term", "STEMCredits")
Include=c("MajCat", "Class")
HSAcademics=c("hsGpa", "hsRank", "HSMTH", "HSSCI")
Demographics=c("Ethnicity", "USctzn", "IARes", "sexCd", "MW_ParentEd")
Activities=c("Sport", "greek")
ISUAwards=c("Carver", "Hixson", "MVP")
MW_AcademicSkills=c("MW_AcademicSkills")
MW_SocialIntegration=c("MW_SocialIntegration")
MW_MajSatisfaction=c("MW_ChangeMaj")
MW_ISUSatisfaction=c("MW_ISUSatisfaction")
MW_MathSciSelfEfficacy=c("MW_MathSciSelfEfficacy")
MW_FinancialConcerns=c("MWFinancialConcerns")
Tests=c("ALEKS_goalAfter", "actCmpst", "satVrbl", "satMath", "actEngl", "actMath", "actRead")
Sem1Classes=c("Phys", "Chem", "Biol", "Math14", "Calc1", "Calc2", "Psych131", "SCICourses")
MidtermGrades=c("STEMMidterm", "MidtermPoints")
ACTInterest=c("ACTINVSTEMMaj", "MajorSurenessAdj")  
LCommunity=c("LC_member")


#Get ROC Curves
DATA14 <- STEM[STEM$dataset_term==2014,]
DATA15 <- STEM[STEM$dataset_term==2015,]
DATA14 <- subset(DATA14, select=-c(anonId, majorCurrStart.1, Sem3begSTEM, EnrS3, dataset_term, PlannedEC, satVrbl, satMath,SCICourses))
DATA15 <- subset(DATA15, select=-c(anonId, majorCurrStart.1, Sem3begSTEM, EnrS3, dataset_term, PlannedEC, satVrbl, satMath,SCICourses))
Academic <- c(HSAcademics, Tests, MidtermGrades, Sem1Classes, Include)
NonAcademic <- c(Activities, MW_AcademicSkills, MW_SocialIntegration, MW_MajSatisfaction, MW_ISUSatisfaction, MW_MathSciSelfEfficacy, MW_FinancialConcerns, ACTInterest,LCommunity, Demographics,ISUAwards, Include)
NoMaj <- unique(c(Academic, NonAcademic))
NoMaj <- NoMaj[NoMaj!="MajCat"]
EABLike <- c(HSAcademics, Tests, Sem1Classes, Include)

DATA14ac <- DATA14[, names(DATA14)%in%Academic]
DATA15ac <- DATA15[, names(DATA15)%in%Academic]
DATA14nac <- DATA14[, names(DATA14)%in%NonAcademic]
DATA15nac <- DATA15[, names(DATA15)%in%NonAcademic]
DATA14NoMaj <- DATA14[, names(DATA14)%in%NoMaj]
DATA15NoMaj <- DATA15[, names(DATA15)%in%NoMaj]
DATA14EAB <- DATA14[, names(DATA14)%in%EABLike]
DATA15EAB <- DATA15[, names(DATA15)%in%EABLike]


set.seed(09182017)
CF=cforest(as.factor(Class)~., data=DATA14, controls=cforest_unbiased(ntree=500, minsplit=75))
CFPred=unlist(predict(CF, newdata=DATA15, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred)
set.seed(09182017)
CF2=cforest(as.factor(Class)~., data=DATA14ac, controls=cforest_unbiased(ntree=500, minsplit=75))
CFPred2=unlist(predict(CF2, newdata=DATA15ac, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred2)
set.seed(09182017)
CF3=cforest(as.factor(Class)~., data=DATA14nac, controls=cforest_unbiased(ntree=500, minsplit=75))
CFPred3=unlist(predict(CF3, newdata=DATA15nac, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred3)
set.seed(09182017)
CF4=cforest(as.factor(Class)~., data=DATA14NoMaj, controls=cforest_unbiased(ntree=500, minsplit=75))
CFPred4=unlist(predict(CF4, newdata=DATA15NoMaj, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred4)
set.seed(09182017)
CF5=cforest(as.factor(Class)~., data=DATA14EAB, controls=cforest_unbiased(ntree=500, minsplit=75))
CFPred5=unlist(predict(CF5, newdata=DATA15EAB, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred5)











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
lines(1-AcademicROC$specificity,AcademicROC$sensitivity,type="l", col="red", ylab="True Positive Rate", xlab="False Positive Rate", main="LR ROC Curves")
NonAcademicROC=ROC(CFPred3, Truth)
lines(1-NonAcademicROC$specificity,NonAcademicROC$sensitivity,type="l", col="green", ylab="True Positive Rate", xlab="False Positive Rate")
legend("bottomright", c("Full Model", "Cognitive Only", "Non-cognitive Only"), lty=c(1,1,1), col=c("blue", "red", "green"), cex=.55)

##############################################################################################

#ROC curves for ISU retention


set.seed(04082017)
setwd("~/Box Sync/Iowa State/Engage Analysis/Current Code/Summer 2017")
load("STEMfiles.Rdata")
STEM <- STEM1[(STEM1$dataset_term>2013) & (STEM1$dataset_term<2016),]
STEM$Class=as.numeric(STEM$Class==2)

exclude=c("anonId", "majorCurrStart.1", "Sem3begSTEM", "EnrS3", "dataset_term", "STEMCredits")
Include=c("MajCat", "Class")
HSAcademics=c("hsGpa", "hsRank", "HSMTH", "HSSCI")
Demographics=c("Ethnicity", "USctzn", "IARes", "sexCd", "MW_ParentEd")
Activities=c("Sport", "greek")
ISUAwards=c("Carver", "Hixson", "MVP")
MW_AcademicSkills=c("MW_AcademicSkills")
MW_SocialIntegration=c("MW_SocialIntegration")
MW_MajSatisfaction=c("MW_ChangeMaj")
MW_ISUSatisfaction=c("MW_ISUSatisfaction")
MW_MathSciSelfEfficacy=c("MW_MathSciSelfEfficacy")
MW_FinancialConcerns=c("MWFinancialConcerns")
Tests=c("ALEKS_goalAfter", "actCmpst", "satVrbl", "satMath", "actEngl", "actMath", "actRead")
Sem1Classes=c("Phys", "Chem", "Biol", "Math14", "Calc1", "Calc2", "Psych131", "SCICourses")
MidtermGrades=c("STEMMidterm", "MidtermPoints")
ACTInterest=c("ACTINVSTEMMaj", "MajorSurenessAdj")  
LCommunity=c("LC_member")


#Get ROC Curves
DATA14 <- STEM[STEM$dataset_term==2014,]
DATA15 <- STEM[STEM$dataset_term==2015,]
DATA14 <- subset(DATA14, select=-c(anonId, majorCurrStart.1, Sem3begSTEM, EnrS3, dataset_term, PlannedEC, satVrbl, satMath,SCICourses))
DATA15 <- subset(DATA15, select=-c(anonId, majorCurrStart.1, Sem3begSTEM, EnrS3, dataset_term, PlannedEC, satVrbl, satMath,SCICourses))
Academic <- c(HSAcademics, Tests, MidtermGrades, Sem1Classes, Include)
NonAcademic <- c(Activities, MW_AcademicSkills, MW_SocialIntegration, MW_MajSatisfaction, MW_ISUSatisfaction, MW_MathSciSelfEfficacy, MW_FinancialConcerns, ACTInterest,LCommunity, Demographics,ISUAwards, Include)
NoMaj <- unique(c(Academic, NonAcademic))
NoMaj <- NoMaj[NoMaj!="MajCat"]
EABLike <- c(HSAcademics, Tests, Sem1Classes, Include)

DATA14ac <- DATA14[, names(DATA14)%in%Academic]
DATA15ac <- DATA15[, names(DATA15)%in%Academic]
DATA14nac <- DATA14[, names(DATA14)%in%NonAcademic]
DATA15nac <- DATA15[, names(DATA15)%in%NonAcademic]
DATA14NoMaj <- DATA14[, names(DATA14)%in%NoMaj]
DATA15NoMaj <- DATA15[, names(DATA15)%in%NoMaj]
DATA14EAB <- DATA14[, names(DATA14)%in%EABLike]
DATA15EAB <- DATA15[, names(DATA15)%in%EABLike]


set.seed(09182017)
CF=cforest(as.factor(Class)~., data=DATA14, controls=cforest_unbiased(ntree=500, minsplit=75))
CFPred=unlist(predict(CF, newdata=DATA15, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred)
set.seed(09182017)
CF2=cforest(as.factor(Class)~., data=DATA14ac, controls=cforest_unbiased(ntree=500, minsplit=75))
CFPred2=unlist(predict(CF2, newdata=DATA15ac, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred2)
set.seed(09182017)
CF3=cforest(as.factor(Class)~., data=DATA14nac, controls=cforest_unbiased(ntree=500, minsplit=75))
CFPred3=unlist(predict(CF3, newdata=DATA15nac, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred3)
set.seed(09182017)
CF4=cforest(as.factor(Class)~., data=DATA14NoMaj, controls=cforest_unbiased(ntree=500, minsplit=75))
CFPred4=unlist(predict(CF4, newdata=DATA15NoMaj, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred4)
set.seed(09182017)
CF5=cforest(as.factor(Class)~., data=DATA14EAB, controls=cforest_unbiased(ntree=500, minsplit=75))
CFPred5=unlist(predict(CF5, newdata=DATA15EAB, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred5)











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
plot(1-AllROC$specificity,AllROC$sensitivity,type="l", col="blue", ylab="True Positive Rate", xlab="False Positive Rate", main="ROC Curves", pch=16)
AcademicROC=ROC(CFPred2, Truth)
lines(1-AcademicROC$specificity,AcademicROC$sensitivity,type="l", col="red", ylab="True Positive Rate", xlab="False Positive Rate", main="LR ROC Curves")
NonAcademicROC=ROC(CFPred3, Truth)
lines(1-NonAcademicROC$specificity,NonAcademicROC$sensitivity,type="l", col="green", ylab="True Positive Rate", xlab="False Positive Rate", main="LR ROC Curves")
legend("bottomright", c("Full Model", "Academic Only", "Nonacademic Only"), lty=c(1,1,1), col=c("blue", "red", "green"), cex=.75)


