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


save.image(file="AUC_VI_STEM_7-2.Rdata")

##########################################################################################
load("AUC_VI_STEM_7-2.Rdata")

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

save(ALLdf, BIOdf, ENGdf, HHdf, MTHdf, PHYdf, file="VISTEM_7-2.Rdata")
###########################################################################




































###########################################################################
#Plots for overall loss
p1=ggplot(data=ALLdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-All STEM", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p2=ggplot(data=BIOdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-BIO", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p3=ggplot(data=ENGdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-ENG", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p4=ggplot(data=HHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-HH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p5=ggplot(data=MTHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-MTH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p6=ggplot(data=PHYdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-PHY", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
multiplot(p1,p2,p3,p4,p5,p6,cols=2)



load("AUCS_STEMdf.Rdata")
library(reshape2)

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
p1=ggplot(data=ALLdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-All STEM", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p2=ggplot(data=BIOdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-BIO", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p3=ggplot(data=ENGdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-ENG", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p4=ggplot(data=HHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-HH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p5=ggplot(data=MTHdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-MTH", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p6=ggplot(data=PHYdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC2, FUN=median), y = -AUCPCTDEC2, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-PHY", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
multiplot(p1,p2,p3,p4,p5,p6,cols=2)

####################################################################################################
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

save.image(file="AUC_VI_5-30_WITHGRAPHS.Rdata")

##############################################################################
##VI for gender and ethnicity
load("AUC_VI_5-30.Rdata")


ComputeAUCS=function(Gender, MajCat){
  AUC1=array(NA, dim=c(ntrees,14))
  AUC2=array(NA, dim=c(ntrees,14))
  #Compute AUC 1 tree and 1 group at a time
  for(t in 1:ntrees){
    AUC1[t,1]=AUC2[t,1]=auc(DATA$Class[DATA$sexCd%in%Gender & DATA$MajCat%in%MajCat],Preds[DATA$sexCd%in%Gender & DATA$MajCat%in%MajCat,t])
    for(g in 1:13){
      AUC1[t,g+1]=auc(DATA$Class[DATA$sexCd%in%Gender & DATA$MajCat%in%MajCat],PredsPerm1[DATA$sexCd%in%Gender & DATA$MajCat%in%MajCat,t,g])
      AUC2[t,g+1]=auc(DATA$Class[DATA$sexCd%in%Gender& DATA$MajCat%in%MajCat],PredsPerm2[DATA$sexCd%in%Gender & DATA$MajCat%in%MajCat,t,g])
    }
  }
  return(list(AUC1, AUC2))
}


VarGroupNames=c("HSAcademics", "Demographics", "Activities", "ISUAwards", "MW_AcademicSkills", "MW_SocialIntegration","MW_Satisfaction", "MW_MathSciSelfEfficacy","MW_FinancialConcerns", "Tests", "Sem1Classes", "MidtermGrades", "ACTInterest" )

ntrees=1000

#BIO by Gender

AUCBIOF=ComputeAUCS(Gender="F", MajCat="BIO")
BIOFdf=data.frame(VarGroupNames,colMeans(AUCBIOF[[1]]-AUCBIOF[[1]][,1])[2:14],100*colMeans((AUCBIOF[[1]]-AUCBIOF[[1]][,1])/AUCBIOF[[1]][,1])[2:14])
names(BIOFdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
BIOFdf$AUCDEC2=colMeans(AUCBIOF[[2]]-AUCBIOF[[2]][,1])[2:14]
BIOFdf$AUCPCTDEC2=100*colMeans((AUCBIOF[[2]]-AUCBIOF[[2]][,1])/AUCBIOF[[2]][,1])[2:14]

AUCBIOM=ComputeAUCS(Gender="M", MajCat="BIO")
BIOMdf=data.frame(VarGroupNames,colMeans(AUCBIOM[[1]]-AUCBIOM[[1]][,1])[2:14],100*colMeans((AUCBIOM[[1]]-AUCBIOM[[1]][,1])/AUCBIOM[[1]][,1])[2:14])
names(BIOMdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
BIOMdf$AUCDEC2=colMeans(AUCBIOM[[2]]-AUCBIOM[[2]][,1])[2:14]
BIOMdf$AUCPCTDEC2=100*colMeans((AUCBIOM[[2]]-AUCBIOM[[2]][,1])/AUCBIOM[[2]][,1])[2:14]

#ENG by Gender

AUCENGF=ComputeAUCS(Gender="F", MajCat="ENG")
ENGFdf=data.frame(VarGroupNames,colMeans(AUCENGF[[1]]-AUCENGF[[1]][,1])[2:14],100*colMeans((AUCENGF[[1]]-AUCENGF[[1]][,1])/AUCENGF[[1]][,1])[2:14])
names(ENGFdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
ENGFdf$AUCDEC2=colMeans(AUCENGF[[2]]-AUCENGF[[2]][,1])[2:14]
ENGFdf$AUCPCTDEC2=100*colMeans((AUCENGF[[2]]-AUCENGF[[2]][,1])/AUCENGF[[2]][,1])[2:14]

AUCENGM=ComputeAUCS(Gender="M", MajCat="ENG")
ENGMdf=data.frame(VarGroupNames,colMeans(AUCENGM[[1]]-AUCENGM[[1]][,1])[2:14],100*colMeans((AUCENGM[[1]]-AUCENGM[[1]][,1])/AUCENGM[[1]][,1])[2:14])
names(ENGMdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
ENGMdf$AUCDEC2=colMeans(AUCENGM[[2]]-AUCENGM[[2]][,1])[2:14]
ENGMdf$AUCPCTDEC2=100*colMeans((AUCENGM[[2]]-AUCENGM[[2]][,1])/AUCENGM[[2]][,1])[2:14]

#HH by Gender

AUCHHF=ComputeAUCS(Gender="F", MajCat="HH")
HHFdf=data.frame(VarGroupNames,colMeans(AUCHHF[[1]]-AUCHHF[[1]][,1])[2:14],100*colMeans((AUCHHF[[1]]-AUCHHF[[1]][,1])/AUCHHF[[1]][,1])[2:14])
names(HHFdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
HHFdf$AUCDEC2=colMeans(AUCHHF[[2]]-AUCHHF[[2]][,1])[2:14]
HHFdf$AUCPCTDEC2=100*colMeans((AUCHHF[[2]]-AUCHHF[[2]][,1])/AUCHHF[[2]][,1])[2:14]

AUCHHM=ComputeAUCS(Gender="M", MajCat="HH")
HHMdf=data.frame(VarGroupNames,colMeans(AUCHHM[[1]]-AUCHHM[[1]][,1])[2:14],100*colMeans((AUCHHM[[1]]-AUCHHM[[1]][,1])/AUCHHM[[1]][,1])[2:14])
names(HHMdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
HHMdf$AUCDEC2=colMeans(AUCHHM[[2]]-AUCHHM[[2]][,1])[2:14]
HHMdf$AUCPCTDEC2=100*colMeans((AUCHHM[[2]]-AUCHHM[[2]][,1])/AUCHHM[[2]][,1])[2:14]


#Plots for overall loss
p1=ggplot(data=BIOFdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-BIO(F)", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p2=ggplot(data=BIOMdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-BIO(M)", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p3=ggplot(data=ENGFdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-ENG(F)", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p4=ggplot(data=ENGMdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-ENG(M)", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p5=ggplot(data=HHFdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-HH(F)", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)
p6=ggplot(data=HHMdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = -AUCPCTDEC, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Variable Importance-HH(M)", y="% Decrease in Performance When Excluded", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,10)

multiplot(p1,p3,p5,p2,p4,p6,cols=2)
###################################################################################
#HardSTEM vs soft STEM Gender

AUCHF=ComputeAUCS(Gender="F", MajCat=c("ENG","MTH", "PHY"))
HFdf=data.frame(VarGroupNames,colMeans(AUCHF[[1]]-AUCHF[[1]][,1])[2:14],100*colMeans((AUCHF[[1]]-AUCHF[[1]][,1])/AUCHF[[1]][,1])[2:14])
names(HFdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
HFdf$AUCDEC2=colMeans(AUCHF[[2]]-AUCHF[[2]][,1])[2:14]
HFdf$AUCPCTDEC2=100*colMeans((AUCHF[[2]]-AUCHF[[2]][,1])/AUCHF[[2]][,1])[2:14]
HFdf$VI=HFdf$AUCPCTDEC
HFdf$VI[HFdf$VI>0]=0
HFdf$VI=HFdf$VI/sum(HFdf$VI)*100

AUCHM=ComputeAUCS(Gender="M", MajCat=c("ENG","MTH", "PHY"))
HMdf=data.frame(VarGroupNames,colMeans(AUCHM[[1]]-AUCHM[[1]][,1])[2:14],100*colMeans((AUCHM[[1]]-AUCHM[[1]][,1])/AUCHM[[1]][,1])[2:14])
names(HMdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
HMdf$AUCDEC2=colMeans(AUCHM[[2]]-AUCHM[[2]][,1])[2:14]
HMdf$AUCPCTDEC2=100*colMeans((AUCHM[[2]]-AUCHM[[2]][,1])/AUCHM[[2]][,1])[2:14]
HMdf$VI=HMdf$AUCPCTDEC
HMdf$VI[HMdf$VI>0]=0
HMdf$VI=HMdf$VI/sum(HMdf$VI)*100


AUCSF=ComputeAUCS(Gender="F", MajCat=c("BIO", "HH"))
SFdf=data.frame(VarGroupNames,colMeans(AUCSF[[1]]-AUCSF[[1]][,1])[2:14],100*colMeans((AUCSF[[1]]-AUCSF[[1]][,1])/AUCSF[[1]][,1])[2:14])
names(SFdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
SFdf$AUCDEC2=colMeans(AUCSF[[2]]-AUCSF[[2]][,1])[2:14]
SFdf$AUCPCTDEC2=100*colMeans((AUCSF[[2]]-AUCSF[[2]][,1])/AUCSF[[2]][,1])[2:14]
SFdf$VI=SFdf$AUCPCTDEC
SFdf$VI[SFdf$VI>0]=0
SFdf$VI=SFdf$VI/sum(SFdf$VI)*100

AUCSM=ComputeAUCS(Gender="M", MajCat=c("BIO", "HH"))
SMdf=data.frame(VarGroupNames,colMeans(AUCSM[[1]]-AUCSM[[1]][,1])[2:14],100*colMeans((AUCSM[[1]]-AUCSM[[1]][,1])/AUCSM[[1]][,1])[2:14])
names(SMdf)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
SMdf$AUCDEC2=colMeans(AUCSM[[2]]-AUCSM[[2]][,1])[2:14]
SMdf$AUCPCTDEC2=100*colMeans((AUCSM[[2]]-AUCSM[[2]][,1])/AUCSM[[2]][,1])[2:14]
SMdf$VI=SMdf$AUCPCTDEC
SMdf$VI[SMdf$VI>0]=0
SMdf$VI=SMdf$VI/sum(SMdf$VI)*100




#Plots for overall loss
p1=ggplot(data=HFdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = VI, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Women in ENG, MTH, PHY", y="Scaled Importance", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,40)
p2=ggplot(data=HMdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = VI, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Men in ENG, MTH, PHY", y="Scaled Importance", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,40)
p3=ggplot(data=SFdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = VI, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Women in BIO, HH", y="Scaled Importance", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,40)
p4=ggplot(data=SMdf, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = VI, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Men in BIO, HH", y="Scaled Importance", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,40)

multiplot(p1,p3,p2,p4,cols=2)
#######################################################################################


####################################################################################
#Plots by Ethnicity and International Student status
load("AUC_VI_5-30.Rdata")


ComputeAUCS=function(EthGrp, MajCat){
  AUC1=array(NA, dim=c(ntrees,14))
  AUC2=array(NA, dim=c(ntrees,14))
  #Compute AUC 1 tree and 1 group at a time
  for(t in 1:ntrees){
    AUC1[t,1]=AUC2[t,1]=auc(DATA$Class[Eth==EthGrp & DATA$MajCat%in%MajCat],Preds[(Eth==EthGrp & DATA$MajCat%in%MajCat),t])
    for(g in 1:13){
      AUC1[t,g+1]=auc(DATA$Class[Eth==EthGrp & DATA$MajCat%in%MajCat],PredsPerm1[Eth==EthGrp & DATA$MajCat%in%MajCat,t,g])
      AUC2[t,g+1]=auc(DATA$Class[Eth==EthGrp & DATA$MajCat%in%MajCat],PredsPerm2[Eth==EthGrp & DATA$MajCat%in%MajCat,t,g])
    }
  }
  return(list(AUC1, AUC2))
}


VarGroupNames=c("HSAcademics", "Demographics", "Activities", "ISUAwards", "MW_AcademicSkills", "MW_SocialIntegration","MW_Satisfaction", "MW_MathSciSelfEfficacy","MW_FinancialConcerns", "Tests", "Sem1Classes", "MidtermGrades", "ACTInterest" )
#Setup vector with ethnicity info
DATA$Ethnicity[DATA$Ethnicity%in%c(7,9)]=NA  #exclude students with 2 or more, or not reporting
Eth=as.numeric(DATA$Ethnicity%in%c(3,4))  #Nonunderrepresented
Eth[DATA$Ethnicity%in%c(1,2,5,6)]=2  #Underrepresented
Eth[DATA$USctzn==0]=3  #International students
DATA=DATA[!is.na(Eth),]
Preds=Preds[!is.na(Eth),]
PredsPerm1=PredsPerm1[!is.na(Eth),,]
PredsPerm2=PredsPerm2[!is.na(Eth),,]
Eth=Eth[!is.na(Eth)]
ntrees=1000

#ALL by Eth
AUCALL0=ComputeAUCS(EthGrp=1, MajCat=c("BIO", "ENG", "HH", "MTH", "PHY"))
ALL0df=data.frame(VarGroupNames,colMeans(AUCALL0[[1]]-AUCALL0[[1]][,1])[2:14],100*colMeans((AUCALL0[[1]]-AUCALL0[[1]][,1])/AUCALL0[[1]][,1])[2:14])
names(ALL0df)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
ALL0df$AUCDEC2=colMeans(AUCALL0[[2]]-AUCALL0[[2]][,1])[2:14]
ALL0df$AUCPCTDEC2=100*colMeans((AUCALL0[[2]]-AUCALL0[[2]][,1])/AUCALL0[[2]][,1])[2:14]
ALL0df$VI=ALL0df$AUCPCTDEC
ALL0df$VI[ALL0df$VI>0]=0
ALL0df$VI=ALL0df$VI/sum(ALL0df$VI)*100

AUCALL1=ComputeAUCS(EthGrp=2, MajCat=c("BIO", "ENG", "HH", "MTH", "PHY"))
ALL1df=data.frame(VarGroupNames,colMeans(AUCALL1[[1]]-AUCALL1[[1]][,1])[2:14],100*colMeans((AUCALL1[[1]]-AUCALL1[[1]][,1])/AUCALL1[[1]][,1])[2:14])
names(ALL1df)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
ALL1df$AUCDEC2=colMeans(AUCALL1[[2]]-AUCALL1[[2]][,1])[2:14]
ALL1df$AUCPCTDEC2=100*colMeans((AUCALL1[[2]]-AUCALL1[[2]][,1])/AUCALL1[[2]][,1])[2:14]
ALL1df$VI=ALL1df$AUCPCTDEC
ALL1df$VI[ALL1df$VI>0]=0
ALL1df$VI=ALL1df$VI/sum(ALL1df$VI)*100

AUCALL2=ComputeAUCS(EthGrp=3, MajCat=c("BIO", "ENG", "HH", "MTH", "PHY"))
ALL2df=data.frame(VarGroupNames,colMeans(AUCALL2[[1]]-AUCALL2[[1]][,1])[2:14],100*colMeans((AUCALL2[[1]]-AUCALL2[[1]][,1])/AUCALL2[[1]][,1])[2:14])
names(ALL2df)=c("VarGroup", "AUCDEC", "AUCPCTDEC")
ALL2df$AUCDEC2=colMeans(AUCALL2[[2]]-AUCALL2[[2]][,1])[2:14]
ALL2df$AUCPCTDEC2=100*colMeans((AUCALL2[[2]]-AUCALL2[[2]][,1])/AUCALL2[[2]][,1])[2:14]
ALL2df$VI=ALL2df$AUCPCTDEC
ALL2df$VI[ALL2df$VI>0]=0
ALL2df$VI=ALL2df$VI/sum(ALL2df$VI)*100

#Plots for overall loss
p1=ggplot(data=ALL0df, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = VI, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "Under Represented Students", y="Scaled Variable Importance", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,40)
p2=ggplot(data=ALL1df, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = VI, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "White and Asian Students", y="Scaled Variable Importance", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,40)
p3=ggplot(data=ALL2df, aes(x = reorder(x=VarGroup, X=-AUCPCTDEC, FUN=median), y = VI, fill="Blue"))  + geom_bar(stat = "identity")+ coord_flip()+theme(legend.position="none")+ labs(title = "International Students", y="Scaled Variable Importance", x="Variable")+ theme(axis.title.y  = element_text(size=16), axis.text.y  = element_text(size=12))+ylim(0,90)

multiplot(p1,p2, p3,cols=1)

