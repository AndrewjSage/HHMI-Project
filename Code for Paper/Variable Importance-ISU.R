library(party)
library(pROC)
library(caret)
library(ggplot2)
################################################################################
#Function to plot multiple graphs together
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

################################################################################


set.seed(04082017)
setwd("~/Box Sync/Iowa State/Engage Analysis/Current Code/Summer 2017")
load("STEMfiles.Rdata")
STEM1 <- STEM1[(STEM1$dataset_term>2013) & (STEM1$dataset_term<2016),]
DATA <- subset(STEM1, select=-c(anonId, majorCurrStart.1, Sem3begSTEM, EnrS3, dataset_term, PlannedEC, satVrbl, satMath,SCICourses))
DATA$Class <-factor(as.numeric(DATA$Class==2))


pctmiss=function(x){
return(mean(complete.cases(x)))  
}

round((1-apply(DATA, 2, pctmiss))*100,2)

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
Academic <- c(HSAcademics, Tests, MidtermGrades, Sem1Classes, Demographics,ISUAwards)
NonAcademic <- c(Activities, MW_AcademicSkills, MW_SocialIntegration, MW_MajSatisfaction, MW_ISUSatisfaction, MW_MathSciSelfEfficacy, MW_FinancialConcerns, ACTInterest,LCommunity, Demographics)
DATA14ac <- DATA14[, names(DATA14)%in%c(Academic, "Class")]
DATA15ac <- DATA15[, names(DATA15)%in%c(Academic, "Class")]
DATA14nac <- DATA14[, names(DATA14)%in%c(NonAcademic, "Class")]
DATA15nac <- DATA15[, names(DATA15)%in%c(NonAcademic, "Class")]

  
set.seed(09182017)
CF=cforest(as.factor(Class)~., data=DATA14, controls=cforest_unbiased(ntree=100, minsplit=75, minbucket=20))
CFPred=unlist(predict(CF, newdata=DATA15, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred)
CF2=cforest(as.factor(Class)~., data=DATA14ac, controls=cforest_unbiased(ntree=100, minsplit=75, minbucket=20))
CFPred2=unlist(predict(CF2, newdata=DATA15ac, type="prob"))[seq(2,2*nrow(DATA15),2)]
auc(as.numeric(as.character(DATA15$Class)), CFPred2)
CF3=cforest(as.factor(Class)~., data=DATA14nac, controls=cforest_unbiased(ntree=100, minsplit=75, minbucket=20))
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
plot(1-AllROC$specificity,AllROC$sensitivity,type="l", col="blue", ylab="True Positive Rate", xlab="False Positive Rate", main="ROC Curves", pch=16)
AcademicROC=ROC(CFPred2, Truth)
lines(1-AcademicROC$specificity,AcademicROC$sensitivity,type="l", col="red", ylab="True Positive Rate", xlab="False Positive Rate", main="LR ROC Curves")
NonAcademicROC=ROC(CFPred3, Truth)
lines(1-NonAcademicROC$specificity,NonAcademicROC$sensitivity,type="l", col="green", ylab="True Positive Rate", xlab="False Positive Rate", main="LR ROC Curves")
legend("bottomright", c("Full Model", "Academic Factors Only", "Non-Academic Factors Only"), lty=c(1,1,1), col=c("blue", "red", "green"), cex=.5)


           









#Function to permute all variables in a certain group
PermuteGroup=function(group){
  dset1=test
  for (j in group){
    permind=sample(1:nrow(dset1), nrow(dset1), replace=F) 
    dset1[1:nrow(dset1),j]=dset1[permind,j]
  }
  return(dset1)
}

DATA$Class=as.factor(DATA$Class)
DATA=DATA[,!names(DATA)%in%exclude]

#Create vectors of numeric indices of columns for each variable in each group
iHSAcademics=which(colnames(DATA)%in%HSAcademics)
iDemographics=which(colnames(DATA)%in%Demographics)
iActivities=which(colnames(DATA)%in%Activities)
iISUAwards=which(colnames(DATA)%in%ISUAwards)
iMW_AcademicSkills=which(colnames(DATA)%in%MW_AcademicSkills)
iMW_SocialIntegration=which(colnames(DATA)%in%MW_SocialIntegration)
iMW_MajSatisfaction=which(colnames(DATA)%in%MW_MajSatisfaction)
iMW_ISUSatisfaction=which(colnames(DATA)%in%MW_ISUSatisfaction)
iMW_MathSciSelfEfficacy=which(colnames(DATA)%in%MW_MathSciSelfEfficacy)
iMW_FinancialConcerns=which(colnames(DATA)%in%MW_FinancialConcerns)
iTests=which(colnames(DATA)%in%Tests)
iSem1Classes=which(colnames(DATA)%in%Sem1Classes)
iMidtermGrades=which(colnames(DATA)%in%MidtermGrades)
iACTInterest=which(colnames(DATA)%in%ACTInterest) 
iLCommunity=which(colnames(DATA)%in%LCommunity) 









STEMDATA=DATA

ntree=1000
nfolds=1
set.index <- createFolds(STEMDATA$MajCat, k=nfolds, list = FALSE)

OOBPreds=array(NA, dim=c(nfolds, nrow(DATA),ntree))
PermOOBPreds=array(NA, dim=c(nfolds, nrow(DATA),ntree, 15))

###############################################################################
#Function to re-predict after permutation

#since we decided not to do the conditional VI, we don't need predsPerm2
PermPredict=function(Group){
  testperm=PermuteGroup(Group)
  #First run back through
  predsPerm1=unlist(predict(CT, testperm, type="prob"))[seq(from=2, to=2*nrow(testperm), by=2)]
  #Then regrow and repredict
 # ct <- ctree(Class~., data=train, controls=ctree_control(mincriterion=0, minsplit = minsp, minbucket = minb, mtry = mt))
#  predsPerm2=unlist(predict(ct, testperm, type="prob"))[seq(from=2, to=2*nrow(testperm), by=2)]
#  return(list(predsPerm1, predsPerm2))
  return(predsPerm1)
  }
############################################################################
set.seed(04192017)
for (i in 1:nfolds){
  DATA=STEMDATA[set.index==i, ]
  Preds=array(NA, dim=c(nrow(DATA), ntree))
  PredsPerm1=array(NA,dim=c(nrow(DATA), ntree, 15))
  #PredsPerm2=array(NA,dim=c(nrow(DATA), ntree, 15))
  MajCat=array(NA, dim=c(nrow(DATA), ntree))
  Truth=array(NA, dim=c(nrow(DATA), ntree))
  TestInd=array(NA, dim=c(nrow(DATA), ntree))
  minsp=75
  minb=20
  mt=7
  for (t in 1:ntree){
    print(t)
    g=1
    subsamp=sample(1:nrow(DATA), floor(nrow(DATA)*.63), replace=F)
    subsamp=subsamp[order(subsamp)]
    testind=which((1:nrow(DATA)%in%subsamp)==FALSE)
    TestInd[testind,t]=1
    train=DATA[subsamp, ]
    test=DATA[testind, ]
    CT <- ctree(Class~., data=train, controls=ctree_control(mincriterion=0,minsplit = minsp, minbucket = minb, mtry = mt))
    preds=unlist(predict(CT, test, type="prob"))[seq(from=2, to=2*nrow(test), by=2)]
    Preds[testind, t]=preds
    Truth[testind,t]=test$Class
    MajCat[testind,t]=test$MajCat
    #Permute iHSAcademics
    PredsPerm1[testind,t,g]=PermPredict(iHSAcademics)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iDemographics)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iActivities)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iISUAwards)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iMW_AcademicSkills)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iMW_SocialIntegration)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iMW_MajSatisfaction)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iMW_ISUSatisfaction)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iMW_MathSciSelfEfficacy)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iMW_FinancialConcerns)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iTests)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iSem1Classes)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iMidtermGrades)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iACTInterest)
    g <- g+1
    PredsPerm1[testind,t,g]=PermPredict(iLCommunity)
    g <- g+1
  }
}


save.image(file="AUC_VI_ISU_7-2.Rdata")

##########################################################################################
setwd("~/Box Sync/Iowa State/Engage Analysis/Science Paper")
load("AUC_VI_ISU_7-2.Rdata")

ComputeAUCS=function(MajCat){
AUC1=array(NA, dim=c(ntrees,16)) #first col is for preds with no groups permuted
#AUC2=array(NA, dim=c(ntrees,16))
#Compute AUC 1 tree and 1 group at a time
for(t in 1:ntrees){
AUC1[t,1]=auc(DATA$Class[DATA$MajCat%in%MajCat],Preds[DATA$MajCat%in%MajCat,t])
for(g in 1:15){
AUC1[t,g+1]=auc(DATA$Class[DATA$MajCat%in%MajCat],PredsPerm1[DATA$MajCat%in%MajCat,t,g])
#AUC2[t,g+1]=auc(DATA$Class[DATA$MajCat%in%MajCat],PredsPerm2[DATA$MajCat%in%MajCat,t,g])
}
}
return(AUC1)
}

VarGroupNames=c("HSAcademics", "Demographics", "Activities", "ISUAwards", "MW_AcademicSkills", "MW_SocialIntegration","MW_MajSatisfaction","MW_ISUSatisfaction", "MW_MathSciSelfEfficacy","MW_FinancialConcerns", "Tests", "Sem1Classes", "MidtermGrades", "ACTInterest","LCommunity" )


#Table A4 comes from ALLdf, BIOdf, etc. 
ntrees=ntree
AUCALL=ComputeAUCS(c("BIO", "ENG", "HH", "MTH", "PHY"))
ALLdf=data.frame(VarGroupNames,colMeans(AUCALL-AUCALL[,1])[2:16],100*colMeans((AUCALL-AUCALL[,1])/AUCALL[,1])[2:16])
names(ALLdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
#ALLdf$AUCDEC2=colMeans(AUCALL[[2]]-AUCALL[[2]][,1])[2:14]
#ALLdf$AUCPCTDEC2=100*colMeans((AUCALL[[2]]-AUCALL[[2]][,1])/AUCALL[[2]][,1])[2:14]

ALLdf$VI=ALLdf$AUCDEC/sum(ALLdf$AUCDEC)*100
ggplot(data=ALLdf, aes(x = reorder(x=VarGroup, X=VI, FUN=median), y = VI, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Predictive Importance: Leaving ISU", y="Scaled Importance", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,60)

AUCBIO=ComputeAUCS("BIO")
BIOdf=data.frame(VarGroupNames,colMeans(AUCBIO-AUCBIO[,1])[2:16],100*colMeans((AUCBIO-AUCBIO[,1])/AUCBIO[,1])[2:16])
names(BIOdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
#BIOdf$AUCDEC2=colMeans(AUCBIO[[2]]-AUCBIO[[2]][,1])[2:14]
#BIOdf$AUCPCTDEC2=100*colMeans((AUCBIO[[2]]-AUCBIO[[2]][,1])/AUCBIO[[2]][,1])[2:14]

AUCENG=ComputeAUCS("ENG")
ENGdf=data.frame(VarGroupNames,colMeans(AUCENG-AUCENG[,1])[2:16],100*colMeans((AUCENG-AUCENG[,1])/AUCENG[,1])[2:16])
names(ENGdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
#ENGdf$AUCDEC2=colMeans(AUCENG[[2]]-AUCENG[[2]][,1])[2:14]
#ENGdf$AUCPCTDEC2=100*colMeans((AUCENG[[2]]-AUCENG[[2]][,1])/AUCENG[[2]][,1])[2:14]


AUCHH=ComputeAUCS("HH")
HHdf=data.frame(VarGroupNames,colMeans(AUCHH-AUCHH[,1])[2:16],100*colMeans((AUCHH-AUCHH[,1])/AUCHH[,1])[2:16])
names(HHdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
#HHdf$AUCDEC2=colMeans(AUCHH[[2]]-AUCHH[[2]][,1])[2:14]
#HHdf$AUCPCTDEC2=100*colMeans((AUCHH[[2]]-AUCHH[[2]][,1])/AUCHH[[2]][,1])[2:14]

AUCMTH=ComputeAUCS("MTH")
MTHdf=data.frame(VarGroupNames,colMeans(AUCMTH-AUCMTH[,1])[2:16],100*colMeans((AUCMTH-AUCMTH[,1])/AUCMTH[,1])[2:16])
names(MTHdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
#MTHdf$AUCDEC2=colMeans(AUCMTH[[2]]-AUCMTH[[2]][,1])[2:14]
#MTHdf$AUCPCTDEC2=100*colMeans((AUCMTH[[2]]-AUCMTH[[2]][,1])/AUCMTH[[2]][,1])[2:14]

AUCPHY=ComputeAUCS("PHY")
PHYdf=data.frame(VarGroupNames,colMeans(AUCPHY-AUCPHY[,1])[2:16],100*colMeans((AUCPHY-AUCPHY[,1])/AUCPHY[,1])[2:16])
names(PHYdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
#PHYdf$AUCDEC2=colMeans(AUCPHY[[2]]-AUCPHY[[2]][,1])[2:14]
#PHYdf$AUCPCTDEC2=100*colMeans((AUCPHY[[2]]-AUCPHY[[2]][,1])/AUCPHY[[2]][,1])[2:14]

save(ALLdf, BIOdf, ENGdf, HHdf, MTHdf, PHYdf, file="VIISU_7-2.Rdata")
###############################################################################






































###############################################################################
load("AUCS_ISUdf.Rdata")

#Plots for overall loss
p1=ggplot(data=ALLdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-All STEM", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,20)
p2=ggplot(data=BIOdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-BIO", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,20)
p3=ggplot(data=ENGdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-ENG", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,20)
p4=ggplot(data=HHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-HH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,20)
p5=ggplot(data=MTHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-MTH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,20)
p6=ggplot(data=PHYdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-PHY", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,20)
multiplot(p1,p2,p3,p4,p5,p6,cols=2)


VarGroupNames=c("High School Courses and Grades", "Demographics", "ISU Activities", "Scholarships", "Academic Skills/Behaviors*", "Social Integration*", "Satisfaction with ISU/Major*", "Math/Science Self Efficacy*", "Financial Concerns*", "Standardized Tests", "First Semester Classes", "First Semester Midterm Grades", "Goal/Interest Survey")
Plotdf=data.frame(BIOdf$AUCDEC, ENGdf$AUCDEC,HHdf$AUCDEC,MTHdf$AUCDEC,PHYdf$AUCDEC)
Plotdf=-Plotdf
Plotdf[Plotdf<0]=0
Plotdf=data.frame(apply(Plotdf, 2, function(x){return(x/sum(x))}))
Order=order(rowMeans(Plotdf), decreasing = TRUE)#reorder so most important come first
Plotdf=Plotdf[Order, ] 

Plotdf1=data.frame(cbind(c(rep(VarGroupNames[Order],5)),c(Plotdf[,1],Plotdf[,2],Plotdf[,3],Plotdf[,4],Plotdf[,5])))
names(Plotdf1)=c("Variable", "Importance")
Plotdf1$Variable=factor(Plotdf1$Variable, levels = VarGroupNames[rev(Order)])
Plotdf1$Major=rep(c("BIO", "ENG", "HH", "MTH", "PHY"), each=13)
Plotdf1$Importance=as.numeric(as.character((Plotdf1$Importance)))
p <- ggplot(Plotdf1, aes(Major, Variable)) + geom_tile(aes(fill = Importance),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue")
p


#Plots for overall loss not picked up by other variables
#p1=ggplot(data=ALLdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-All STEM", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
#p2=ggplot(data=BIOdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-BIO", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
#p3=ggplot(data=ENGdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-ENG", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
#p4=ggplot(data=HHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-HH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
#p5=ggplot(data=MTHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-MTH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
#p6=ggplot(data=PHYdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-PHY", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
#multiplot(p1,p2,p3,p4,p5,p6,cols=2)

####################################################################################################
#Now aggregate probabilities first and then compute AUC (this is the way I was doing it, which isn't as good)

dset=DATA

CalcMean=function(x){
  return(mean(x, na.rm=T))
}

CalcAUC=function(x){
  return(auc(Truth, x))
}

OOBPred=apply(Preds,1,CalcMean)
OOBPreds1=apply(PredsPerm1, c(1,3), CalcMean)
OOBPreds2=apply(PredsPerm2, c(1,3), CalcMean)

#All students
Truth=DATA$Class
OrigAUC=auc(DATA$Class, OOBPred)
PermAUC=apply(OOBPreds1, 2, CalcAUC)
OVRDiff=PermAUC-OrigAUC
ALLdf$AUCDEC3=OVRDiff
ALLdf$AUCPCTDEC3=OVRDiff/OrigAUC*100
PermAUC=apply(OOBPreds2, 2, CalcAUC)
OVRDiff=PermAUC-OrigAUC
ALLdf$AUCDEC4=OVRDiff
ALLdf$AUCPCTDEC4=OVRDiff/OrigAUC*100

#BIO
Truth=DATA$Class[DATA$MajCat=="BIO"]
OrigAUC=auc(Truth, OOBPred[DATA$MajCat=="BIO"])
PermAUC=apply(OOBPreds1[DATA$MajCat=="BIO",], 2, CalcAUC)
BIODiff=PermAUC-OrigAUC
BIOdf$AUCDEC3=BIODiff
BIOdf$AUCPCTDEC3=BIODiff/OrigAUC*100
PermAUC=apply(OOBPreds2[DATA$MajCat=="BIO",], 2, CalcAUC)
BIODiff=PermAUC-OrigAUC
BIOdf$AUCDEC4=BIODiff
BIOdf$AUCPCTDEC4=BIODiff/OrigAUC*100


#ENG
Truth=DATA$Class[DATA$MajCat=="ENG"]
OrigAUC=auc(Truth, OOBPred[DATA$MajCat=="ENG"])
PermAUC=apply(OOBPreds1[DATA$MajCat=="ENG",], 2, CalcAUC)
ENGDiff=PermAUC-OrigAUC
ENGdf$AUCDEC3=ENGDiff
ENGdf$AUCPCTDEC3=ENGDiff/OrigAUC*100
PermAUC=apply(OOBPreds2[DATA$MajCat=="ENG",], 2, CalcAUC)
ENGDiff=PermAUC-OrigAUC
ENGdf$AUCDEC4=ENGDiff
ENGdf$AUCPCTDEC4=ENGDiff/OrigAUC*100

#HH
Truth=DATA$Class[DATA$MajCat=="HH"]
OrigAUC=auc(Truth, OOBPred[DATA$MajCat=="HH"])
PermAUC=apply(OOBPreds1[DATA$MajCat=="HH",], 2, CalcAUC)
HHDiff=PermAUC-OrigAUC
HHdf$AUCDEC3=HHDiff
HHdf$AUCPCTDEC3=HHDiff/OrigAUC*100
PermAUC=apply(OOBPreds2[DATA$MajCat=="HH",], 2, CalcAUC)
HHDiff=PermAUC-OrigAUC
HHdf$AUCDEC4=HHDiff
HHdf$AUCPCTDEC4=HHDiff/OrigAUC*100

#MTH
Truth=DATA$Class[DATA$MajCat=="MTH"]
OrigAUC=auc(Truth, OOBPred[DATA$MajCat=="MTH"])
PermAUC=apply(OOBPreds1[DATA$MajCat=="MTH",], 2, CalcAUC)
MTHDiff=PermAUC-OrigAUC
MTHdf$AUCDEC3=MTHDiff
MTHdf$AUCPCTDEC3=MTHDiff/OrigAUC*100
PermAUC=apply(OOBPreds2[DATA$MajCat=="MTH",], 2, CalcAUC)
MTHDiff=PermAUC-OrigAUC
MTHdf$AUCDEC4=MTHDiff
MTHdf$AUCPCTDEC4=MTHDiff/OrigAUC*100

#PHY
Truth=DATA$Class[DATA$MajCat=="PHY"]
OrigAUC=auc(Truth, OOBPred[DATA$MajCat=="PHY"])
PermAUC=apply(OOBPreds1[DATA$MajCat=="PHY",], 2, CalcAUC)
PHYDiff=PermAUC-OrigAUC
PHYdf$AUCDEC3=PHYDiff
PHYdf$AUCPCTDEC3=PHYDiff/OrigAUC*100
PermAUC=apply(OOBPreds2[DATA$MajCat=="PHY",], 2, CalcAUC)
PHYDiff=PermAUC-OrigAUC
PHYdf$AUCDEC4=PHYDiff
PHYdf$AUCPCTDEC4=PHYDiff/OrigAUC*100

#Plots for overall loss
p1=ggplot(data=ALLdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC3, FUN=median), y = -AUCPCTDEC3, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-All STEM", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p2=ggplot(data=BIOdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC3, FUN=median), y = -AUCPCTDEC3, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-BIO", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p3=ggplot(data=ENGdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC3, FUN=median), y = -AUCPCTDEC3, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-ENG", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p4=ggplot(data=HHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC3, FUN=median), y = -AUCPCTDEC3, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-HH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p5=ggplot(data=MTHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC3, FUN=median), y = -AUCPCTDEC3, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-MTH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p6=ggplot(data=PHYdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC3, FUN=median), y = -AUCPCTDEC3, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-PHY", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
multiplot(p1,p2,p3,p4,p5,p6,cols=2)


#Plots for overall loss not picked up by other variables
p1=ggplot(data=ALLdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC4, FUN=median), y = -AUCPCTDEC4, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-All STEM", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p2=ggplot(data=BIOdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC4, FUN=median), y = -AUCPCTDEC4, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-BIO", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p3=ggplot(data=ENGdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC4, FUN=median), y = -AUCPCTDEC4, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-ENG", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p4=ggplot(data=HHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC4, FUN=median), y = -AUCPCTDEC4, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-HH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p5=ggplot(data=MTHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC4, FUN=median), y = -AUCPCTDEC4, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-MTH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p6=ggplot(data=PHYdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC4, FUN=median), y = -AUCPCTDEC4, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-PHY", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
multiplot(p1,p2,p3,p4,p5,p6,cols=2)

save.image(file="AUC_VI6-1_WITHGRAPHS_ISU.Rdata")
