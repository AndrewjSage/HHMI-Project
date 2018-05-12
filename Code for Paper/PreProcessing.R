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
STEMmaj <- subset(stem.codes,(STEM=="Yes"))$Inst.major.code
MAJ$Sem1begSTEM <- as.integer(MAJ$majorCurrStart.1%in%STEMmaj)
MAJ$Sem3begSTEM <- as.integer(MAJ$majorCurrStart.3%in%STEMmaj)
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

load("Mapworks.Rdata")
STEM <- merge(STEM, subset(MW, MW$anonId%in%STEM$anonId), all=T)

ALEKS <- read.csv("ALEKS_All.csv")
names(ALEKS)[names(ALEKS)=="goalAfter"] <- "ALEKS_goalAfter"
varnames <- c("anonId", "ALEKS_goalAfter")
ALEKS <- ALEKS[,varnames]

STEM <- merge(STEM, subset(ALEKS, ALEKS$anonId%in%STEM$anonId), all=T)

#Learning Communities
LC <- read.csv("Learning Communities_All.csv")
#Only want LC info for first year #This code excludes more advanced students who show up in LC data
LC14 <- subset(LC, LC$dataset_term==2014 & LC$anonId%in%subset(Students, Students$dataset_term==2014)$anonId)
LC15 <- subset(LC, LC$dataset_term==2015 & LC$anonId%in%subset(Students, Students$dataset_term==2015)$anonId)
LC <- rbind(LC14, LC15)
names(LC)[names(LC)=="member"] <- "LC_member"
varnames <- c("anonId", "LC_member")
LC <- LC[,varnames]

STEM <- merge(STEM, subset(LC, LC$anonId%in%STEM$anonId),by="anonId", all=T)
STEM[is.na(STEM$LC_member),]$LC_member <- 0

#Act Inventory variables
ACTInv <- read.csv("ACT Inventory_All.csv")
ACTInv$ACTINVSTEMMaj <- c(rep(NA, nrow(ACTInv)))
ACTInv$ACTINVSTEMMaj[ACTInv$actSpQstn011 %in% c(110:134, 415, 421, 422, 460:472, 529, 532, 540:553, 560:583,610:770, 860:875, 898, 899)] <- 1 #stem majors
ACTInv$ACTINVSTEMMaj[!ACTInv$actSpQstn011 %in% c(110:134, 415, 421, 422, 460:472, 529, 532, 540:553, 560:583,610:770, 860:875, 898, 899)] <- 0
ACTInv$ACTINVSTEMMaj[is.na(ACTInv$actSpQstn011)] <- NA
ACTInv$ACTINVSTEMMaj <- as.numeric(as.character(ACTInv$ACTINVSTEMMaj))
ACTInv$PlannedEC <- rowSums((ACTInv[,24:36]==1),na.rm=T) #Number of Activities student plans to participate in during College ques 40-55
ACTInv$HSEC <- rowSums((ACTInv[,77:89]==1),na.rm=T) ##Number of HS Extracurriculars ques 99-114
ACTInv$MajorSurenessAdj <- ACTInv$actSpQstn014
ACTInv$MajorSurenessAdj[ACTInv$ACTINVSTEMMaj==0] <- NA
varnames <- c("anonId", "ACTINVSTEMMaj","PlannedEC","MajorSurenessAdj")
ACTInv <- ACTInv[,names(ACTInv)%in%varnames]

STEM <- merge(STEM, subset(ACTInv, ACTInv$anonId%in%STEM$anonId),by="anonId", all=T)

#students, i.e. those in Students for appropriate year
Courses14 <- rbind(read.xls("Grades2014a.xls"), read.xls("Grades2014b.xls"),read.xls("Grades2014c.xls"))
Courses14 <- Courses14[Courses14$anonId%in%Students[Students$dataset_term=="2014",]$anonId,]
Courses15 <- rbind(read.xls("Grades2015a.xls"), read.xls("Grades2015b.xls"),read.xls("Grades2015c.xls"))
Courses15 <- Courses15[Courses15$anonId%in%Students[Students$dataset_term=="2015",]$anonId,]
Courses <- rbind(Courses14, Courses15)
Courses <- Courses[Courses$anonId%in%STEM$anonId,]
#Exclude 1-credit courses
Courses <- subset(Courses,credits>1)
#Count number of courses by department
Courses$Phys <- as.numeric(Courses$dept=="PHYS")
Courses$Chem <- as.numeric(Courses$dept=="CHEM")
Courses$Biol <- as.numeric(Courses$dept=="BIOL")
#number of math 140 level courses
Courses$Math14 <- as.numeric(Courses$dept=="MATH" & (Courses$course=="140" |Courses$course=="141" |Courses$course=="142"))
Courses$Calc1 <- as.numeric(Courses$courseTitle=="CALCULUS I")
Courses$Calc2 <- as.numeric(Courses$courseTitle=="CALCULUS II")
Courses$Psych131 <- as.numeric(Courses$dept=="PSYCH" | Courses$course=="131")
Courses$C_Grade <- as.numeric(Courses$midtermGrade=="C-")
Courses$D_Grade <- as.numeric(Courses$midtermGrade=="D")
Courses$F_Grade <- as.numeric(Courses$midtermGrade=="F")
#Reshape to get a dataset with students as rows and number of courses as columns
library(plyr)
#Courses1=ddply(Courses, .(anonId), summarise, Phys=sum(Phys), Chem=sum(Chem), Biol=sum(Biol), Math14=sum(Math14), Calc1=sum(Calc1), Calc2=sum(Calc2), Psych131=sum(Psych131), MidtermPoints=sum(C_Grade)+sum(D_Grade)+sum(F_Grade))
Courses1 <- ddply(Courses, .(anonId), summarise, Phys=sum(Phys), Chem=sum(Chem), Biol=sum(Biol), Math14=sum(Math14), Calc1=sum(Calc1), Calc2=sum(Calc2), Psych131=sum(Psych131), MidtermPoints=sum(C_Grade)+2*sum(D_Grade)+3*sum(F_Grade))
Courses1$Math14 <- as.numeric(Courses1$Math14>0)        #Treat math14 and Psych131 and 0-1 indicator of taking these remedial courses
Courses1$Psych131 <- as.numeric(Courses1$Psych131>0)
Courses1$SCICourses <- Courses1$Phys+Courses1$Chem+Courses1$Biol
stem.codes <- read.csv("stem codes.csv")
depts <- read.csv("depts.csv")
STEMdepts <- depts$Dept[depts$STEM==1]
STEMCourses <- Courses[Courses$dept%in%STEMdepts,]
STEMCourses$Pts <- STEMCourses$C_Grade+2*STEMCourses$D_Grade+3*STEMCourses$F_Grade
STEMCredits <- ddply(STEMCourses, .(anonId), summarise,  STEMCredits=sum(credits))
Courses2 <- ddply(STEMCourses, .(anonId), summarise,  STEMMidterm=mean(Pts))
#Courses2$STEMCredits[is.na(Courses2$STEMCredits)]=0
Courses1 <- merge(Courses1, Courses2, by="anonId", all=TRUE)
Courses1 <- merge(Courses1, STEMCredits, by="anonId", all=TRUE)

STEM1 <- merge(STEM, subset(Courses1, Courses1$anonId%in%STEM$anonId),by="anonId", all=T)
STEM <- STEM1[STEM1$Class<2,]

setwd("~/Box Sync/Iowa State/Engage Analysis/Paper/Data and R Code/RData Files")
save(STEM, STEM1, file="STEMfiles.Rdata")
