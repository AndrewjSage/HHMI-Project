setwd("~/Box Sync/Iowa State/Engage Analysis/Paper/Data and R Code/Datasets_2016")
library(gdata)
library(plyr)
library(mice)

#First Identify students we want to look at
FYstudents=read.csv("Student Information_All.csv")
Years=c(2014,2015)
Studentids=FYstudents[FYstudents$dataset_term%in%Years,]$anonId
stem.codes <- read.csv("stem codes.csv")
F2014 <- read.csv("Student Info By Semester_Fall_2014.csv")
F2015 <- read.csv("Student Info By Semester_Fall_2015.csv")
F2016 <- read.csv("Student Info By Semester_Fall_2016.csv")
S2015 <- read.csv("Student Info By Semester_Spring_2015.csv")
S2016 <- read.csv("Student Info By Semester_Spring_2016.csv")
S2017 <- subset(S2016, S2016$anonId%in%c("NA"))     #empty dataset for bookkeeping reasons 
#Combine student info including majors and GPA for fall, spring terms
MAJ14 <- merge(F2014, S2015, by="anonId", all=TRUE)
MAJ15 <- merge(F2015, S2016, by="anonId", all=TRUE)
MAJ16 <- merge(F2016, S2017, by="anonId", all=TRUE)
MAJ17 <- MAJ16             #Want everyone since we don't know who leaves 
#Get majors for 2nd year
MAJ14 <- merge(MAJ14, subset(MAJ15, MAJ15$anonId%in%MAJ14$anonId), by="anonId", all=TRUE)
MAJ15 <- merge(MAJ15, subset(MAJ16, MAJ16$anonId%in%MAJ15$anonId), by="anonId", all=TRUE)
MAJ16 <- merge(MAJ16, subset(MAJ17, MAJ17$anonId%in%MAJ16$anonId), by="anonId", all=TRUE)
#Change names of variables to correspond to semester numbers
Semnums=function(df){
  names(df) <- gsub("x.x", "1", names(df))
  names(df) <- gsub("y.x", "2", names(df))
  names(df) <- gsub("x.y", "3", names(df))
  names(df) <- gsub("y.y", "4", names(df))
  return(df)
}
MAJ14 <- Semnums(MAJ14)
MAJ15 <- Semnums(MAJ15)
MAJ16 <- Semnums(MAJ16)
#Restrict to first year students
MAJ14 <- subset(MAJ14, MAJ14$anonId%in%subset(FYstudents, FYstudents$dataset_term==2014)$anonId)
MAJ15 <- subset(MAJ15, MAJ15$anonId%in%subset(FYstudents, FYstudents$dataset_term==2015)$anonId)
MAJ16 <- subset(MAJ16, MAJ16$anonId%in%subset(FYstudents, FYstudents$dataset_term==2016)$anonId)
#Dataset containing major information for all students
MAJ <- rbind(MAJ14, MAJ15, MAJ16)
#Determine whether students were STEM majors initially and after 1 year
STEMmaj<-subset(stem.codes,(STEM=="Yes"))$Inst.major.code
MAJ$Sem1begSTEM<-as.integer(MAJ$majorCurrStart.1%in%STEMmaj)
MAJ$Sem3begSTEM<-as.integer(MAJ$majorCurrStart.3%in%STEMmaj)
#Determine whether students were enrolled at ISU at start of 2nd, 3rd, 4th semester
ENGclass <- subset(stem.codes,Class=="ENG")$Inst.major.code
BIOclass <- subset(stem.codes,Class=="BIO")$Inst.major.code
PHYclass <- subset(stem.codes,Class=="PHY")$Inst.major.code
HHclass <- subset(stem.codes,Class=="HH")$Inst.major.code
MTHclass <- subset(stem.codes,Class=="MTH")$Inst.major.code
MAJ$MajCat <- rep(NA, nrow(MAJ))
MAJ$MajCat[MAJ$majorCurrStart.1%in%ENGclass] <- "ENG"
MAJ$MajCat[MAJ$majorCurrStart.1%in%BIOclass] <- "BIO"
MAJ$MajCat[MAJ$majorCurrStart.1%in%PHYclass] <- "PHY"
MAJ$MajCat[MAJ$majorCurrStart.1%in%HHclass] <- "HH"
MAJ$MajCat[MAJ$majorCurrStart.1%in%MTHclass] <- "MTH"
MAJ$MajCat <- as.factor(MAJ$MajCat)
MAJ$EnrS3<-as.integer(!is.na(MAJ$majorCurrStart.3))

STEMstudents <- MAJ[MAJ$Sem1begSTEM==1,]
STEMstudents <- merge(STEMstudents, FYstudents, by=c("anonId"))
STEMstudents <- STEMstudents[STEMstudents$majorCurrStart.1!="OPEN",]
STEMstudents <- STEMstudents[STEMstudents$dataset_term%in%Years,]
STEMstudents <- STEMstudents[STEMstudents$admsnType==1,]
Students <- STEMstudents[,c(1,6, 47:85)]
HSMTH <- Students$hsAlgbrUnits+Students$hsGeomUnits+Students$hsTrigUnits+Students$hsCalcUnits
HSSCI <- Students$hsScnceUnits+Students$hsChemUnits+Students$hsPhysUnits+Students$hsBioUnits
HSMTH[HSMTH>15] <- 15  #some numbers are way to high to be real
HSSCI[HSSCI>15] <- 15 #some numbers are way to high to be real
HSMTH[HSMTH==0] <- NA  #some numbers are way to high to be real
HSSCI[HSSCI==0] <- NA #some numbers are way to high to be real

Age <- Students$dataset_term-Students$birthyear
Students$ethncRptGrp[is.na(Students$ethncRptGrp==T)] <- 9 #category for international students
Students$ethncRptGrp[Students$ethncRptGrp=="8"] <- NA #Set those who didn't indicate to NA
Ethnicity <- as.factor(Students$ethncRptGrp)
Sport <- NA
Sport[Students$athlcSportCd==""] <- 0
Sport[Students$athlcSportCd!=""] <- 1
USctzn <- as.numeric(as.character(Students$ctznCd)=="Y")
#Create 0-1 variable for IA residency
IARes <- Students$resCd<200
#Set native language to eng/noneng
EngNatLang <- as.numeric(Students$englNatvLangInd=="E")
#Split Hixon categories
Carver <- as.numeric(Students$hixsonAward=="C"|Students$hixsonAward=="J")
Hixson <- as.numeric(Students$hixsonAward=="H"|Students$hixsonAward=="I")
MVP <- as.numeric(Students$hixsonAward=="M"|Students$hixsonAward=="I")

Class <- 1-Students$Sem3begSTEM
Class[Students$EnrS3==0] <- 2

STEM <- Students[,c(1:6,11:15, 17, 24, 39:41  )]
STEM$HSMTH <- HSMTH
STEM$HSSCI <- HSSCI
STEM$Ethnicity <- Ethnicity
STEM$Sport <- Sport
STEM$USctzn <- USctzn
STEM$IARes <- as.numeric(IARes)
STEM$Carver <- Carver
STEM$Hixson <- Hixson
STEM$MVP <- MVP
STEM$Class <- Class

#######################################################################
#Mapworks stuff-can skip after 1st run
MW <- read.csv("MapworksData.csv")
MW <- MW[MW$anonId%in%STEMstudents$anonId,]
MW[MW==99] <- NA
exclude=c("q118", "offcampusStudy", "offcampusSleep", "offcampusSleep", "offcampusSatisfy","X", "anonId", "dataset_term.x")
MWFA <- MW[,!names(MW)%in%exclude]
#Impute Missing values
set.seed(04092017)
MWFA <- mice(MWFA, m = 1, method = vector("character", length = ncol(MWFA)),
             predictorMatrix = (1 - diag(1, ncol(MWFA))),
             visitSequence = (1:ncol(MWFA))[apply(is.na(MWFA), 2, any)],
             form = vector("character", length = ncol(MWFA)),
             post = vector("character", length = ncol(MWFA)), defaultMethod = c("pmm",
                                                                                "logreg", "polyreg", "polr"), maxit = 5, diagnostics = TRUE,
             printFlag = TRUE, seed = NA, imputationMethod = NULL,
             defaultImputationMethod = NULL, data.init = NULL)
MWIMP <- complete(MWFA,1)

factorinfo <- factanal(MWIMP, 5, rotation="promax", scale=T, center=T)
#print(factorinfo, digits=2, cutoff=0.2, sort=TRUE)
#factorinfo$loadings[,1:5]

MW_AcademicSkills <- as.matrix(MWIMP)%*%as.matrix(factorinfo$loadings[,1])
MW_SocialIntegration <- as.matrix(MWIMP)%*%as.matrix(factorinfo$loadings[,2])
MW_ISUSatisfaction <- as.matrix(MWIMP)%*%as.matrix(factorinfo$loadings[,3])
MW_MathSciSelfEfficacy <- as.matrix(MWIMP)%*%as.matrix(factorinfo$loadings[,4])
MW_FinancialConcerns <- as.matrix(MWIMP)%*%as.matrix(factorinfo$loadings[,5])
MWFA <- data.frame(MW_AcademicSkills, MW_SocialIntegration, MW_ISUSatisfaction, MW_MathSciSelfEfficacy, MW_FinancialConcerns)

#Switch from mother/father ed. levels to highest parent ed. level since this is how question is asked after 2016
MW$d002[MW$d002>4] <- NA
MW$d003[MW$d003>4] <- NA
MW$d002[is.na(MW$d002)] <- -1
MW$d003[is.na(MW$d003)] <- -1
MW_ParentEd <- apply(data.frame(MW$d002, MW$d003),1,max)
MW_ParentEd[MW_ParentEd==-1] <- NA
MW_ChangeMaj <- MW$changemajor
anonId <- MW$anonId

MW <- cbind(anonId, MWFA, MW_ParentEd, MW_ChangeMaj)
setwd("~/Box Sync/Iowa State/Engage Analysis/Paper/Data and R Code/Datasets_2016")
save(MW, file="Mapworks.Rdata")
