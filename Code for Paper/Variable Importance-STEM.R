#trees grown using R version 3.4.3 (2017-11-30)
#party_1.2-4
#pROC_1.10.0
#mice_2.46.0


library(party)
library(pROC)
library(caret)
library(ggplot2)

set.seed(04082017)
setwd("~/Box Sync/Iowa State/Engage Analysis/Paper/Data and R Code/RData Files")
load("STEMfiles.Rdata")  
#In STEM, Should have 3240 students in 2014 and 3344 in 2015
#In STEM1, Should have 3667 in 2014 and 3792 in 2015
STEM <- STEM[(STEM$dataset_term>2013) & (STEM$dataset_term<2016),]
DATA <- subset(STEM, select=-c(anonId, majorCurrStart.1, Sem3begSTEM, EnrS3, dataset_term, PlannedEC, satVrbl, satMath,SCICourses))


exclude=c("anonId", "majorCurrStart.1", "Sem3begSTEM", "EnrS3", "dataset_term", "STEMCredits")
Include=c("MajCat", "Class")
HSAcademics=c("hsGpa", "hsRank", "HSMTH", "HSSCI")
Demographics=c("Ethnicity", "USctzn", "IARes", "sexCd", "MW_ParentEd")
Activities=c("Sport", "greek", "PlannedEC")  #didn't end up using PlannedEC. Doesn't hurt to have it though.
ISUAwards=c("Carver", "Hixson", "MVP")
MW_AcademicSkills=c("MW_AcademicSkills")
MW_SocialIntegration=c("MW_SocialIntegration")
MW_MajSatisfaction=c("MW_ChangeMaj")
MW_ISUSatisfaction=c("MW_ISUSatisfaction")
MW_MathSciSelfEfficacy=c("MW_MathSciSelfEfficacy")
MW_FinancialConcerns=c("MWFinancialConcerns")
Tests=c("ALEKS_goalAfter", "actCmpst", "satVrbl", "satMath", "actEngl", "actMath", "actRead") #got rid of SAT's
Sem1Classes=c("Phys", "Chem", "Biol", "Math14", "Calc1", "Calc2", "Psych131", "SCICourses")
MidtermGrades=c("STEMMidterm", "MidtermPoints")
ACTInterest=c("ACTINVSTEMMaj", "MajorSurenessAdj")  
LCommunity=c("LC_member")

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


save.image(file="AUC_VI_STEM.Rdata")

##########################################################################################
load("AUC_VI_STEM.Rdata")

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

save(ALLdf, BIOdf, ENGdf, HHdf, MTHdf, PHYdf, file="VISTEM.Rdata")
###########################################################################
