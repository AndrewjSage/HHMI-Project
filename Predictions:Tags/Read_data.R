setwd("~/Box Sync/Iowa State/Engage Analysis/Datasets")

##Factor analysis on Mapworks data
MW11<-read.csv("Mapworks_2011.csv")
MW12<-read.csv("Mapworks_2012.csv")
MW13<-read.csv("Mapworks_2013.csv")
MW14<-read.csv("Mapworks_2014.csv")
MW15<-read.csv("Mapworks_2015.csv")
MW16<-read.csv("Mapworks_2016.csv")
MW11$dataset_term.x="2011"
MW12$dataset_term.x="2012"
MW13$dataset_term.x="2013"
MW14$dataset_term.x="2014"
MW15$dataset_term.x="2015"
MW16$dataset_term.x="2016"
MW=rbind(MW11, MW12, MW13, MW14, MW15, MW16)
#Exclude factors MW has already computed. Questions align differently from year to year
MW=MW[,-(71:91)]


setwd("~/Box Sync/Iowa State/Engage Analysis/Datasets")
write.csv(MW, "MapworksData.csv")

#Exclude variables that are not on a Likert Scale, have missing data, or we otherwise don't want to include for some reason
FAvars=c("financetuition", "finalcemonthlyliving", "q141","q140","q145","q143","q144","q125", "q126", "q099", "q101", "q139", "q142", "q094", "q092", "q091", "q096", "q097", "q098", "q142", "q090", "q025", "q027", "q065", "q066", "q067", "q071", "q068", "q034", "q058", "q070", "q135", "q073", "q028", "q036", "q059", "q136", "q030", "q031", "q077", "q082", "q138", "q076", "q078", "q084", "q074")
MWFA=MW[, (names(MW) %in% FAvars)]
#Replace 0's and 99's with NA's
#Note all questions in MWFA should not have 0 for a response. Interpret as missing.
MWFA[MWFA==0]=NA
MWFA[MWFA==99]=NA
pctmissing=data.frame(round(apply(is.na(MWFA),2,mean),2))

#Impute missing values using Multivariate Imputation using Chained Equations
library(mice)
MWFA=mice(MWFA, m = 5, method = vector("character", length = ncol(MWFA)),
               predictorMatrix = (1 - diag(1, ncol(MWFA))),
               visitSequence = (1:ncol(MWFA))[apply(is.na(MWFA), 2, any)],
               form = vector("character", length = ncol(MWFA)),
               post = vector("character", length = ncol(MWFA)), defaultMethod = c("pmm",
                                                                                    "logreg", "polyreg", "polr"), maxit = 5, diagnostics = TRUE,
               printFlag = TRUE, seed = NA, imputationMethod = NULL,
               defaultImputationMethod = NULL, data.init = NULL)
Imputeddata=complete(MWFA,5)
#Imputeddata$anonId=MW$anonId
setwd("~/Box Sync/Iowa State/Engage Analysis/Datasets")
write.csv(Imputeddata, "MW_IMP2016.csv")

#################################################################################################################################################
#Can start here if we already have MW imputed data
setwd("~/Box Sync/Iowa State/Engage Analysis/Datasets")
MW=read.csv("MapworksData.csv")
MWFA=read.csv("MW_IMP2016.csv")[,-1]

factorinfo <- factanal(MWFA, 5, rotation="promax", scale=T, center=T)
print(factorinfo, digits=2, cutoff=0.2, sort=TRUE)
factorinfo$loadings[,1:5]

MW_AcademicSkills=as.matrix(MWFA)%*%as.matrix(factorinfo$loadings[,1])
MW_SocialIntegration=as.matrix(MWFA)%*%as.matrix(factorinfo$loadings[,2])
MW_PeerConnections=as.matrix(MWFA)%*%as.matrix(factorinfo$loadings[,3])
MW_MathSciSelfEfficacy=as.matrix(MWFA)%*%as.matrix(factorinfo$loadings[,4])
MW_FinancialConcerns=as.matrix(MWFA)%*%as.matrix(factorinfo$loadings[,5])
MWFA=data.frame(MW_AcademicSkills, MW_SocialIntegration, MW_PeerConnections, MW_MathSciSelfEfficacy, MW_FinancialConcerns)

#Switch from mother/father ed. levels to highest parent ed. level since this is how question is asked after 2016
MW$d002[MW$d002>4]=NA
MW$d003[MW$d003>4]=NA
MW$d002[is.na(MW$d002)]=-1
MW$d003[is.na(MW$d003)]=-1
ParentEd=apply(data.frame(MW$d002, MW$d003),1,max)
ParentEd[ParentEd==-1]=NA
MW$d002=ParentEd

#Include certain MW variables that were not included in factor analysis
include=c("anonId", "d002","d004","d042","d052","d055","na132", "changemajor")
MW=MW[,include]
MW[MW==99]=NA
names(MW)=c("anonId","MW_Parent_Ed","MW_ISU_rank","MW_Aspired_Edu_Level","MW_HS_hrs_studying","MW_Courses_struggling","MW_College_hrs_studying", "MW_ChangeMaj")
MW$MW_ChangeMaj[MW$MW_ChangeMaj==0]=NA
MW=cbind(MW,MWFA )

#Student Information for first years
FYinfo=read.csv("Student Information_All.csv")
FYinfo$ethncRptGrp[is.na(FYinfo$ethncRptGrp==T)]=9 #category for international students
FYinfo$ethncRptGrp[FYinfo$ethncRptGrp=="8"]=NA #Set those who didn't indicate to NA
FYinfo$ethncRptGrp=as.factor(FYinfo$ethncRptGrp)
FYinfo$smrTrialAdmit=1*(FYinfo$smrTrialAdmit=="Y")
#Separate variables for Hixson Award
FYinfo$Carver=as.numeric(FYinfo$hixsonAward=="C"|FYinfo$hixsonAward=="J")
FYinfo$Hixson=as.numeric(FYinfo$hixsonAward=="H"|FYinfo$hixsonAward=="I")
FYinfo$MVP=as.numeric(FYinfo$hixsonAward=="M"|FYinfo$hixsonAward=="I")
#Create Y/N variable for whether student participates in sport at ISU
FYinfo$Sport[FYinfo$athlcSportCd==""]=0
FYinfo$Sport[FYinfo$athlcSportCd!=""]=1
FYinfo$Age=as.numeric(FYinfo$dataset_term)-FYinfo$birthyear
FYinfo$sexCd=as.numeric(FYinfo$sexCd=="F")
#Break residency into appropriate categories
FYinfo$USctzn=as.character(FYinfo$ctznCd)
FYinfo$USctzn=as.numeric(as.character(FYinfo$USctzn)=="Y")
#Create 0-1 variable for IA residency
FYinfo$IAres=FYinfo$resCd<200
#Set native language to eng/noneng
FYinfo$EngNatLang=as.numeric(FYinfo$englNatvLangInd=="E")
addzeros <- function(df){
  for (i in 23:34){                 
    df[,i][is.na(df[,i])]<-0          #Replace NAs with 0's
  }
  Allzeros=rowSums(df[,23:34]==0)
  df[Allzeros==12, 23:34] <- rep(NA,12)     #for students with no class info, set to NA
  return(df)
}
FYinfo <- addzeros(FYinfo)
#Pick out variables to include in model
varnames=c("anonId", "admsnType", "dataset_term", "actCmpst", "hsRank", "hsGpa", "sexCd", "ethncRptGrp", "EngNatLang", "curRai", "greek", "smrTrialAdmit", "hsArtUnits", "hsSclScnceUnits", "hsScnceUnits", "hsAlgbrUnits", "hsGeomUnits", "hsTrigUnits", "hsCalcUnits", "hsChemUnits", "hsPhysUnits","hsBioUnits", "hsEnglUnits", "hsEnglUnits", "hsSpnshUnits", "Hixson", "Carver", "MVP", "Sport", "Age", "IAres", "EngNatLang" )
FYinfo=FYinfo[,names(FYinfo)%in%varnames]

#ALEKS
ALEKS=read.csv("ALEKS_All.csv")
names(ALEKS)[names(ALEKS)=="goalAfter"]="ALEKS_goalAfter"
varnames=c("anonId", "ALEKS_goalAfter")
ALEKS=ALEKS[,varnames]

#Learning Communities
LC=read.csv("Learning Communities_All.csv")
#Only want LC info for first year #This code excludes more advanced students who show up in LC data
LC11=subset(LC, LC$dataset_term==2011 & LC$anonId%in%subset(FYinfo, FYinfo$dataset_term==2011)$anonId)
LC12=subset(LC, LC$dataset_term==2012 & LC$anonId%in%subset(FYinfo, FYinfo$dataset_term==2012)$anonId)
LC13=subset(LC, LC$dataset_term==2013 & LC$anonId%in%subset(FYinfo, FYinfo$dataset_term==2013)$anonId)
LC14=subset(LC, LC$dataset_term==2014 & LC$anonId%in%subset(FYinfo, FYinfo$dataset_term==2014)$anonId)
LC15=subset(LC, LC$dataset_term==2015 & LC$anonId%in%subset(FYinfo, FYinfo$dataset_term==2015)$anonId)
LC16=subset(LC, LC$dataset_term==2016 & LC$anonId%in%subset(FYinfo, FYinfo$dataset_term==2016)$anonId)
LC=rbind(LC11, LC12, LC13, LC14, LC15, LC16)
levels(LC$type)=c("B","C", "O", "R","0")
names(LC)[names(LC)=="type"]="LC_type"
varnames=c("anonId", "LC_type")
LC=LC[,varnames]

#Act Inventory variables
ACTInv=read.csv("ACT Inventory_All.csv")
ACTInv$ACTINVSTEMMaj=c(rep(NA, nrow(ACTInv)))
ACTInv$ACTINVSTEMMaj[ACTInv$actSpQstn011 %in% c(110:134, 460:472, 529, 532, 540:553, 560:583, 860:875)]=1 #stem majors
ACTInv$ACTINVSTEMMaj[!ACTInv$actSpQstn011 %in% c(110:134, 460:472, 529, 532, 540:553, 560:583, 860:875)]=0
ACTInv$ACTINVSTEMMaj[is.na(ACTInv$actSpQstn011)]=NA
ACTInv$ACTINVSTEMMaj=as.numeric(as.character(ACTInv$ACTINVSTEMMaj))
ACTInv$PlannedEC=rowSums((ACTInv[,24:36]==1),na.rm=T) #Number of Activities student plans to participate in during College ques 40-55
ACTInv$HSEC=rowSums((ACTInv[,77:89]==1),na.rm=T) ##Number of HS Extracurriculars ques 99-114
ACTInv$MajorSurenessAdj=ACTInv$actSpQstn014
ACTInv$MajorSurenessAdj[ACTInv$ACTINVSTEMMaj==0]=NA
ACTInv$ACTINV_ROTC=ACTInv$actSpQstn018
varnames=c("anonId", "ACTINVSTEMMaj","PlannedEC","HSEC","MajorSurenessAdj", "ACTINV_ROTC")
ACTInv=ACTInv[,names(ACTInv)%in%varnames]

library(gdata)
#Courses Data
#Read in courses, which contain all students in all years. Need to restrict to first year STEM
#students, i.e. those in FYinfo for appropriate year
Courses11 <- rbind(read.xls("Grades2011a.xls"), read.xls("Grades2011b.xls"),read.xls("Grades2011c.xls"))
Courses11 <- Courses11[Courses11$anonId%in%FYinfo[FYinfo$dataset_term=="2011",]$anonId,]
Courses12 <- rbind(read.xls("Grades2012a.xls"), read.xls("Grades2012b.xls"),read.xls("Grades2012c.xls"))
Courses12 <- Courses12[Courses12$anonId%in%FYinfo[FYinfo$dataset_term=="2012",]$anonId,]
Courses13 <- rbind(read.xls("Grades2013a.xls"), read.xls("Grades2013b.xls"),read.xls("Grades2013c.xls"))
Courses13 <- Courses13[Courses13$anonId%in%FYinfo[FYinfo$dataset_term=="2013",]$anonId,]
Courses14 <- rbind(read.xls("Grades2014a.xls"), read.xls("Grades2014b.xls"),read.xls("Grades2014c.xls"))
Courses14 <- Courses14[Courses14$anonId%in%FYinfo[FYinfo$dataset_term=="2014",]$anonId,]
Courses15 <- rbind(read.xls("Grades2015a.xls"), read.xls("Grades2015b.xls"),read.xls("Grades2015c.xls"))
Courses15 <- Courses15[Courses15$anonId%in%FYinfo[FYinfo$dataset_term=="2015",]$anonId,]
Courses16 <- rbind(read.xls("Grades2016a.xls"), read.xls("Grades2016b.xls"),read.xls("Grades2016c.xls"))
Courses16<-Courses16[Courses16$anonId%in%FYinfo[FYinfo$dataset_term=="2016",]$anonId,]
Courses=rbind(Courses11,Courses12,Courses13,Courses14, Courses15,Courses16)
#Exclude 1-credit courses
Courses=subset(Courses,credits>1)
#Count number of courses by department
Courses$Phys=as.numeric(Courses$dept=="PHYS")
Courses$Chem=as.numeric(Courses$dept=="CHEM")
Courses$Biol=as.numeric(Courses$dept=="BIOL")
#number of math 140 level courses
Courses$Math14=as.numeric(Courses$dept=="MATH" & (Courses$course=="140" |Courses$course=="141" |Courses$course=="142"))
Courses$Calc1=as.numeric(Courses$courseTitle=="CALCULUS I")
Courses$Calc2=as.numeric(Courses$courseTitle=="CALCULUS II")
Courses$Psych131=as.numeric(Courses$dept=="PSYCH" | Courses$course=="131")
Courses$C_Grade=as.numeric(Courses$midtermGrade=="C-")
Courses$D_Grade=as.numeric(Courses$midtermGrade=="D")
Courses$F_Grade=as.numeric(Courses$midtermGrade=="F")
#Reshape to get a dataset with students as rows and number of courses as columns
library(plyr)
#Courses1=ddply(Courses, .(anonId), summarise, Phys=sum(Phys), Chem=sum(Chem), Biol=sum(Biol), Math14=sum(Math14), Calc1=sum(Calc1), Calc2=sum(Calc2), Psych131=sum(Psych131), MidtermPoints=sum(C_Grade)+sum(D_Grade)+sum(F_Grade))
Courses1=ddply(Courses, .(anonId), summarise, Phys=sum(Phys), Chem=sum(Chem), Biol=sum(Biol), Math14=sum(Math14), Calc1=sum(Calc1), Calc2=sum(Calc2), Psych131=sum(Psych131), MidtermPoints=sum(C_Grade)+2*sum(D_Grade)+3*sum(F_Grade))
Courses1$Math14=as.numeric(Courses1$Math14>0)        #Treat math14 and Psych131 and 0-1 indicator of taking these remedial courses
Courses1$Psych131=as.numeric(Courses1$Psych131>0)
Courses1$StemCourses=Courses1$Phys+Courses1$Chem+Courses1$Biol
stem.codes <- read.csv("stem codes.csv")
depts<-read.csv("depts.csv")
STEMdepts=depts$Dept[depts$STEM==1]
STEMCourses=Courses[Courses$dept%in%STEMdepts,]
STEMCourses$Pts=STEMCourses$C_Grade+2*STEMCourses$D_Grade+3*STEMCourses$F_Grade
Courses2=ddply(STEMCourses, .(anonId), summarise,  STEMMidterm=mean(Pts))
Courses1 <- merge(Courses1, Courses2,  by="anonId", all=TRUE)

#Read in student information concerning all students for each semester
F2011 <- read.csv("Student Info By Semester_Fall_2011.csv")
F2012 <- read.csv("Student Info By Semester_Fall_2012.csv")
F2013 <- read.csv("Student Info By Semester_Fall_2013.csv")
F2014 <- read.csv("Student Info By Semester_Fall_2014.csv")
F2015 <- read.csv("Student Info By Semester_Fall_2015.csv")
F2016 <- read.csv("Student Info By Semester_Fall_2016.csv")
S2012 <- read.csv("Student Info By Semester_Spring_2012.csv")
S2013 <- read.csv("Student Info By Semester_Spring_2013.csv")
S2014 <- read.csv("Student Info By Semester_Spring_2014.csv")
S2015 <- read.csv("Student Info By Semester_Spring_2015.csv")
S2016 <- read.csv("Student Info By Semester_Spring_2016.csv")
S2017 <- subset(S2016, S2016$anonId%in%c("NA"))     #empty dataset for bookkeeping reasons 
#Combine student info including majors and GPA for fall, spring terms
MAJ11 <- merge(F2011, S2012, by="anonId", all=TRUE)
MAJ12 <- merge(F2012, S2013, by="anonId", all=TRUE)
MAJ13 <- merge(F2013, S2014, by="anonId", all=TRUE)
MAJ14 <- merge(F2014, S2015, by="anonId", all=TRUE)
MAJ15 <- merge(F2015, S2016, by="anonId", all=TRUE)
MAJ16 <- merge(F2016, S2017, by="anonId", all=TRUE)
MAJ17 <- MAJ16             #Want everyone since we don't know who leaves 
#Get majors for 2nd year
MAJ11 <- merge(MAJ11, subset(MAJ12, MAJ12$anonId%in%MAJ11$anonId), by="anonId", all=TRUE)
MAJ12 <- merge(MAJ12, subset(MAJ13, MAJ13$anonId%in%MAJ12$anonId), by="anonId", all=TRUE)
MAJ13 <- merge(MAJ13, subset(MAJ14, MAJ14$anonId%in%MAJ13$anonId), by="anonId", all=TRUE)
MAJ14 <- merge(MAJ14, subset(MAJ15, MAJ15$anonId%in%MAJ14$anonId), by="anonId", all=TRUE)
MAJ15 <- merge(MAJ15, subset(MAJ16, MAJ16$anonId%in%MAJ15$anonId), by="anonId", all=TRUE)
MAJ16 <- merge(MAJ16, subset(MAJ17, MAJ17$anonId%in%MAJ16$anonId), by="anonId", all=TRUE)
#Change names of variables to correspond to semester numbers
Semnums=function(df){
  names(df)=gsub("x.x", "1", names(df))
  names(df)=gsub("y.x", "2", names(df))
  names(df)=gsub("x.y", "3", names(df))
  names(df)=gsub("y.y", "4", names(df))
  return(df)
}
MAJ11=Semnums(MAJ11)
MAJ12=Semnums(MAJ12)
MAJ13=Semnums(MAJ13)
MAJ14=Semnums(MAJ14)
MAJ15=Semnums(MAJ15)
MAJ16=Semnums(MAJ16)
#Restrict to first year students
MAJ11=subset(MAJ11, MAJ11$anonId%in%subset(FYinfo, FYinfo$dataset_term==2011)$anonId)
MAJ12=subset(MAJ12, MAJ12$anonId%in%subset(FYinfo, FYinfo$dataset_term==2012)$anonId)
MAJ13=subset(MAJ13, MAJ13$anonId%in%subset(FYinfo, FYinfo$dataset_term==2013)$anonId)
MAJ14=subset(MAJ14, MAJ14$anonId%in%subset(FYinfo, FYinfo$dataset_term==2014)$anonId)
MAJ15=subset(MAJ15, MAJ15$anonId%in%subset(FYinfo, FYinfo$dataset_term==2015)$anonId)
MAJ16=subset(MAJ16, MAJ16$anonId%in%subset(FYinfo, FYinfo$dataset_term==2016)$anonId)
#Dataset containing major information for all students
MAJ=rbind(MAJ11, MAJ12, MAJ13, MAJ14, MAJ15, MAJ16)
#Determine whether students were STEM majors initially and after 1 year
STEMmaj<-subset(stem.codes,(STEM=="Yes"))$Inst.major.code
MAJ$Sem1begSTEM<-as.integer(MAJ$majorCurrStart.1%in%STEMmaj)
MAJ$Sem3begSTEM<-as.integer(MAJ$majorCurrStart.3%in%STEMmaj)
#Determine whether students were enrolled at ISU at start of 2nd, 3rd, 4th semester
MAJ$EnrS3<-as.integer(!is.na(MAJ$majorCurrStart.3))
ENGclass=subset(stem.codes,Class=="ENG")$Inst.major.code
BIOclass=subset(stem.codes,Class=="BIO")$Inst.major.code
PHYclass=subset(stem.codes,Class=="PHY")$Inst.major.code
HHclass=subset(stem.codes,Class=="HH")$Inst.major.code
MTHclass=subset(stem.codes,Class=="MTH")$Inst.major.code
OPENclass=subset(stem.codes,(Class=="OPEN"))$Inst.major.code
MAJ$MajCat=rep(NA, nrow(MAJ))
MAJ$MajCat[MAJ$majorCurrStart.1%in%ENGclass]="ENG"
MAJ$MajCat[MAJ$majorCurrStart.1%in%BIOclass]="BIO"
MAJ$MajCat[MAJ$majorCurrStart.1%in%PHYclass]="PHY"
MAJ$MajCat[MAJ$majorCurrStart.1%in%HHclass]="HH"
MAJ$MajCat[MAJ$majorCurrStart.1%in%OPENclass]="OPEN"
MAJ$MajCat[MAJ$majorCurrStart.1%in%MTHclass]="MTH"
MAJ$MajCat=as.factor(MAJ$MajCat)
#Variable for first semester at ISU
varnames=c("anonId", "clsfnYr.1", "Sem1begSTEM", "Sem3begSTEM", "EnrS3", "MajCat", "majorCurrStart.1")
MAJ=MAJ[, varnames]

#Merge datasets together
#FYinfo contains all the first-year students we want. Only get subsets from other datasets that have these students
INFO=merge(FYinfo,subset(ALEKS,ALEKS$anonId%in%FYinfo$anonId), by="anonId", all=TRUE)
INFO=merge(INFO,subset(LC, LC$anonId%in%INFO$anonId), by="anonId",all=TRUE)
INFO=merge(INFO,subset(ACTInv, ACTInv$anonId%in%INFO$anonId),by="anonId", all=TRUE)
INFO=merge(INFO, subset(Courses1, Courses1$anonId%in%INFO$anonId), by="anonId", all=FALSE) #exclude students that registrar didn't provide course data for
INFO=merge(INFO, subset(MW, MW$anonId%in%INFO$anonId), by="anonId", all=TRUE)
FYStudents=merge(INFO,subset(MAJ, MAJ$anonId%in%INFO$anonId),by="anonId", all=TRUE)
FYStudents=subset(FYStudents, Sem1begSTEM==1) #Only initial STEM majors
#FYStudents=subset(FYStudents, admsnType==1)  #Exclude transfers
FYStudents$Class=1-FYStudents$Sem3begSTEM
FYStudents$LC_type=as.character(FYStudents$LC_type)
FYStudents$LC_type[is.na(FYStudents$LC_type)==T]="0"
FYStudents$LC_type <- factor (as.character(FYStudents$LC_type), levels=c("0","B","C", "O", "R"))

exclude=c("admsnType", "Sem1begSTEM", "Sem3begSTEM")
STEM1=FYStudents[, !(names(FYStudents)%in%exclude)]
STEM1=subset(STEM1, MajCat!="OPEN")
STEM=subset(STEM1, EnrS3==1)      #Only those enrolled in S3
STEM=STEM[, !(names(STEM)%in%c("EnrS3"))]
STEM1=STEM1[, !(names(STEM1)%in%c("EnrS3"))]

changevarnames=function(df){
  names(df)=c("anonId",
              "Year",
              "ACT Composite Score",
              "High School Rank",
              "High School GPA",
              "Gender",
              "Ethnicity",
              "Regent's Admissions Index (RAI)",
              "Greek Life Participation",
              "Summer Trial Admit",
              "High School Art Units",
              "High School Social Science Units",
              "High School Science Units",
              "High School Algebra Units",
              "High School Geometry Units",
              "High School Trigonometry Units",
              "High School Calculus Units",
              "High School Chemistry Units",
              "High School Physics Units",
              "High School Biology Units",
              "High School English Units",
              "High School Spanish Units",
              "Carver Award Recipient",
              "Hixson Award Recipient",
              "MVP Award Recipient",
              "ISU Athlete",
              "Age",
              "Iowa Resident",
              "Native English Speaker",
              "ALEKS Math Placement Overall Score",
              "Learning Community Participation",
              "Selected STEM Major on ACT Interest Survey",
              "Number of Planned Extra-Curriculars",
              "Numer of High School Extra-Curriculars",
              "Sureness of STEM Major on ACT Interest Survey",
              "Interest in ROTC on ACT Interest Survey",
              "Number of 1st Semester Physics Units",
              "Number of 1st Semester Chemistry Units",
              "Number of 1st Semester Biology Units",
              "Enrollment in Math 140 Level Course",
              "Enrollment in Calculus I",
              "Enrollment in Calculus II",
              "Enrollment in PSYCH 131",
              "Midterm Grades",
              "Number of STEM Courses",
              "Midterms STEM Grades",
              "Parent Education Level",
              "Mapworks-ISU Rank in Choices of College",
              "Mapworks-Aspired Level of Education",
              "Mapworks-Hours Studying per week in High School",
              "Mapworks-Number of Courses Struggling in",
              "Mapworks-Hours Studying per Week",
              "Mapworks-Likelihood of Changing Major",
              "Mapworks-Academic Skills",
              "Mapworks-Social Integration",
              "Mapworks-Peer Connections",
              "Mapworks-Math and Science Self Efficacy",
              "Mapworks-Financial Concerns",
              "First-year Classification",
              "Major Category",
              "Major",
              "Class")
  return(df)
}

STEM=changevarnames(STEM)
STEM1=changevarnames(STEM1)



#Check on percentage of missing values
#pctmissing=data.frame(round(apply(is.na(STEM),2,mean),2))
#pctmissing

setwd("~/Box Sync/Iowa State/Engage Analysis/Datasets")
saveRDS(STEM,"STEM.rds") #Contains variables to be used for RF
write.csv(STEM,"STEM.csv")
saveRDS(STEM1,"STEM1.rds") #Contains variables to be used for RF
write.csv(STEM1,"STEM1.csv")
