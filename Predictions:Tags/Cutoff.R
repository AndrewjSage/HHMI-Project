
setwd("~/Box Sync/Iowa State/Engage Analysis/Datasets")
STEMall=readRDS("STEM1.rds")
STEM=readRDS("STEM.rds")

#Leave Probabilities for those who left ISU
set.seed(10262016)
TRAIN=subset(STEM, Year%in%c(2013,2014))
TRAIN1=TRAIN[,c(3:60,62)]  #exclude aninID, term, and Major
TEST=subset(STEM, Year ==2015)
TESTall=subset(STEMall, Year ==2015)
TESTleft=TESTall[!(TESTall$anonId%in%TEST$anonId),]
ntrees=1000
CF=cforest(as.factor(Class)~., data=TRAIN1, controls=cforest_unbiased(ntree=ntrees, minsplit=25, minbucket=0))
CFPrbAll=unlist(predict(CF, newdata=TESTall, type="prob"))[seq(2,2*nrow(TESTleft),2)]

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
#########################################################################################
#Cutoffs by Major category

setwd("~/Box Sync/Iowa State/Engage Analysis/Current Code")
##################################################################
#Get data easily for table

TESTall$Class[!TESTall$anonId%in%TEST$anonId]=2
TESTall$Class=as.factor(TESTall$Class)

prop.table(table(STEMall$`Major Category`, STEMall$Class),1)
