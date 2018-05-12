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

#change names that show up in tree so they make senese
names(DATA) <-c(
  "MAJOR",                 "ACT(C).",               "hsRank",                 "HS GPA",                 
  "sexCd",                  "greek",                  "actEngl",                "ACT-M",               
  "actRead",                "SEM.MTH",                  "HSSCI",                  "Ethnicity",             
  "Sport",                  "USctzn",                 "IARes",                  "Carver",                
  "Hixson",                 "MVP",                    "Class",                  "MW_AcademicSkills",     
  "MW_SocialIntegration",   "MW_ISUSatisfaction",     "MW_MathSciSelfEfficacy", "MW_FinancialConcerns",  
  "MW_ParentEd",            "MAJ.CHG",           "ALEKS_goalAfter",        "LC",             
  "STEM-INTD",          "MAJ.SURE",       "Phys",                   "Chem",                  
  "Biol",                   "Math14",                 "Calc1",                  "Calc2",                 
  "Psych131",               "MidtermPoints",          "STEM-MT",            "STEMCredits"           
)

set.seed(04082017)
CT <- ctree(Class~., data=DATA, controls=ctree_control(mincriterion=0.95,minsplit = 75, minbucket = 20, mtry = 39))
plot(CT, type="simple", inner_panel=node_inner(CT, abbreviate=FALSE, pval = FALSE, id=FALSE), terminal_panel=node_terminal(CT, abbreviate=FALSE,id=FALSE, digits=2))

