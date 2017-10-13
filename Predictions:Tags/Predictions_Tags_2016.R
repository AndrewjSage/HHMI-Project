library(party)
setwd("~/Box Sync/Iowa State/Engage Analysis/Datasets")
STEMall=readRDS("STEM1.rds")
STEM=readRDS("STEM.rds")

#Leave Probabilities for those who left ISU
set.seed(10262016)
TRAIN=subset(STEM, Year%in%c(2014,2015))
TRAIN1=TRAIN[,c(3:60,62)]  #exclude aninID, term, and Major
TEST=subset(STEM, Year ==2016)
TESTall=subset(STEMall, Year ==2016)
TESTleft=TESTall[!(TESTall$anonId%in%TEST$anonId),]
ntrees=1000
CF=cforest(as.factor(Class)~., data=TRAIN1, controls=cforest_unbiased(ntree=ntrees, minsplit=25, minbucket=0))
CFPrbAll=unlist(predict(CF, newdata=TESTall, type="prob"))[seq(2,2*nrow(TESTall),2)]

library(pROC)
auc(TEST$Class, CFPrb)
#CFPrb_uw=UnweightCF(CF,TRAIN,TEST,ntrees)[,2]
#Identify at-risk students

TEST=TESTall
TEST$Prb=CFPrbAll
TESTBIO=subset(TEST, `Major Category`=="BIO")
TESTENG=subset(TEST, `Major Category`=="ENG")
TESTHH=subset(TEST, `Major Category`=="HH")
TESTMTH=subset(TEST, `Major Category`=="MTH")
TESTPHY=subset(TEST, `Major Category`=="PHY")

IDatrisk=function(df, Num){
  Cutoff=-sort(-df$Prb)[Num]
  AtRisk=df[df$Prb>=Cutoff, ]
  return(AtRisk)
}

ARBIO=IDatrisk(TESTBIO, 0.036*nrow(TESTBIO))
ARENG=IDatrisk(TESTENG, 0.066*nrow(TESTENG))
ARHH=IDatrisk(TESTHH, 0.236*nrow(TESTHH))
ARMTH=IDatrisk(TESTMTH, 0.416*nrow(TESTMTH))
ARPHY=IDatrisk(TESTPHY, 0.263*nrow(TESTPHY))

AtRisk=rbind(ARBIO, ARENG,ARHH, ARMTH, ARPHY)
setwd("~/Box Sync/Iowa State/Engage Analysis/Predictions")
save(AtRisk, file="AtRisk2016.Rdata")

##################################################
load("AtRisk2016.Rdata")
numericTags=c("Midterm Grades", "ALEKS Math Placement Overall Score", "Mapworks-Likelihood of Changing Major", "High School GPA", "Mapworks-Math and Science Self Efficacy", "ACT Composite Score","High School Calculus Units", "High School Chemistry Units", "High School Physics Units", "Mapworks-Aspired Level of Education")
Tagdf=AtRisk[,names(AtRisk)%in%numericTags]
Tagdf[,c(7,9)]=-Tagdf[,c(7,9)]
TagStd=data.frame(apply(Tagdf, 2, scale))
TagStd$Category=AtRisk$`Major Category`
Majors=c("ENGR"	,"M E",	"AER E",	"AN S",	"CH E",	"KIN H",	"CPR E",	"C E",	"P CS",	"E E",	"PSYCH",	"BIOL",	"BIOLA",	"S E",	"I E")
TagStd$Category=as.character(TagStd$Category)
TagStd$Category[AtRisk$Major%in%Majors]=as.character(AtRisk$Major[AtRisk$Major%in%Majors])
TagStd$Category=as.factor(TagStd$Category)
TagStd$`Lack of Learning Community Participation`=-5*as.numeric(AtRisk$`Learning Community Participation`==0)
TagStd$`Enrollment in Math 140 Level Course`=-5*as.numeric(AtRisk$`Enrollment in Math 140 Level Course`>0)
TagStd$ID=AtRisk$anonId
names(TagStd)

AERE=TagStd[TagStd$Category=="AER E",c(6,3,7,10,5,14)]
ANS=TagStd[TagStd$Category=="AN S", c(9,1,2,13,7,14)]
BIO=TagStd[TagStd$Category=="BIO",c(9,7,2,6,10,14)]
BIOL=TagStd[TagStd$Category=="BIOL",c(6,2,1,4,7,14)]
BIOLA=TagStd[TagStd$Category=="BIOLA",c(6,9,1,10,7,14)]
CE=TagStd[TagStd$Category=="C E",c(7,6,3,2,10,14)]
CHE=TagStd[TagStd$Category=="CH E",c(13,6,2,4,7,14)]
CPRE=TagStd[TagStd$Category=="CPR E",c(6,7,1,10,2,14)]
EE=TagStd[TagStd$Category=="E E",c(7,1,10,2,6,14)]
ENG=TagStd[TagStd$Category=="ENG",c(7,6,10,2,5,14)]
ENGR=TagStd[TagStd$Category=="ENGR",c(7,6,10,2,5,14)]
HH=TagStd[TagStd$Category=="HH",c(12,7,4,2,8,14)]
IE=TagStd[TagStd$Category=="I E",c(7,4,13,2,10,14)]
KINH=TagStd[TagStd$Category=="KIN H",c(10,9,4,6,7,14)]
ME=TagStd[TagStd$Category=="M E",c(7,2,1,6,10,14)]
MTH=TagStd[TagStd$Category=="MTH",c(9,6,7,3,1,14)]
PCS=TagStd[TagStd$Category=="P CS",c(1,6,3,8,7,14)]
PHY=TagStd[TagStd$Category=="PHY",c(7,9,10,8,6,14)]
PSYCH=TagStd[TagStd$Category=="PSYCH",c(8,12,7,9,2,14)]
SE=TagStd[TagStd$Category=="S E",c(1,10,6,7,2,14)]

TagStudents=function(DF){
DF$Tag1=NA
DF$Tag2=NA
for (i in 1:nrow(DF)){
  DF$Tag1[i]=names(which.min(DF[i,1:5]))
if (sum(DF[i,1:5]<=-0.5, na.rm=T)>1){  #If there are 2 or more factors with z-score <-0.5 then include 2nd tag
  DF$Tag2[i]=names(which(rank(DF[i,1:5])==2))
}
}
return(DF[,6:8])
}

StudentTags=rbind(TagStudents(AERE),TagStudents(ANS), TagStudents(BIO), TagStudents(BIOL), TagStudents(BIOLA), TagStudents(CE), TagStudents(CHE), TagStudents(CPRE), TagStudents(EE), TagStudents(ENG),TagStudents(ENGR), TagStudents(HH), TagStudents(IE), TagStudents(KINH), TagStudents(ME), TagStudents(MTH),TagStudents(PCS), TagStudents(PHY), TagStudents(PSYCH), TagStudents(SE))
StudentTags$Tag1=gsub("\\.", " ", StudentTags$Tag1)
StudentTags$Tag2=gsub("\\.", " ", StudentTags$Tag2)
setwd("~/Box Sync/Iowa State/Engage Analysis/Predictions")
write.csv(StudentTags, "StudentTags2016.csv", row.names=FALSE)


Missing=subset(AtRisk, !AtRisk$anonId%in%StudentTags$ID)
# E E students didn't show up 