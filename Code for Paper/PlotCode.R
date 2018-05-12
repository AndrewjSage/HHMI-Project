########################################################################################
#Table with information for all majors
setwd("~/Box Sync/Iowa State/Engage Analysis/Paper/Data and R Code/RData Files")
load("VISTEM.Rdata")

ALLdf$RelVI <- ALLdf$VI/sum(ALLdf$VI)
data.frame(ALLdf[,1], round(ALLdf[,5],4))

load("VI_ISU.Rdata")
ALLdf$RelVI <- ALLdf$VI/sum(ALLdf$VI)
data.frame(ALLdf[,1], round(ALLdf[,5],4))
Order <- order(ALLdf$RelVI, decreasing = TRUE)#reorder so most important come first
ALLdf <- ALLdf[Order,] 
#######################################################################################
####Leaving ISU
library(ggplot2)
setwd("~/Box Sync/Iowa State/Engage Analysis/Paper/Data and R Code/RData Files")
load("VI_ISU.Rdata")



VarGroupNames=c("High School Courses and Grades", "Demographics", "College Activities", "Scholarships", "Academic Skills/Behaviors*", "Social Integration*", "Satisfaction with Major*","Satisfaction with University*", "Math/Science Self Efficacy*", "Financial Concerns*", "Standardized Tests", "First Semester Classes", "First Semester Midterm Grades", "ACT Interest Survey", "Learning Community Participation")
Plotdf=data.frame(BIOdf$AUCDEC, ENGdf$AUCDEC,HHdf$AUCDEC,MTHdf$AUCDEC,PHYdf$AUCDEC)
#Plotdf=data.frame(BIOdf$AUCPCTDEC, ENGdf$AUCPCTDEC,HHdf$AUCPCTDEC,MTHdf$AUCPCTDEC,PHYdf$AUCPCTDEC)
Plotdf=-Plotdf
Plotdf[Plotdf<0]=0
Plotdf=data.frame(apply(Plotdf, 2, function(x){return(x/sum(x))}))
#Order=order(rowMeans(Plotdf), decreasing = TRUE)#reorder so most important come first
Plotdf=Plotdf[Order, ] 

Plotdf2=data.frame(cbind(c(rep(VarGroupNames[Order],5)),c(Plotdf[,1],Plotdf[,2],Plotdf[,3],Plotdf[,4],Plotdf[,5])))
names(Plotdf2)=c("Variable", "Importance")
Plotdf2$Variable=factor(Plotdf2$Variable, levels = VarGroupNames[rev(Order)])
Plotdf2$Major=rep(c("BIO", "ENG", "HH", "MTH", "PHY"), each=15)
Plotdf2$Importance=as.numeric(as.character((Plotdf2$Importance)))
p2 <- ggplot(Plotdf2, aes(Major, Variable)) + geom_tile(aes(fill = Importance),colour = "white") + scale_fill_gradient(low = "white",high = "red")+ggtitle("Predictiveness of Institutional Retention")+theme(legend.position = "none")+ theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
p2

########Leaving STEM
setwd("~/Box Sync/Iowa State/Engage Analysis/Paper/Data and R Code/RData Files")
load("VISTEM.Rdata")
library(reshape2)

#Plotdf=data.frame(BIOdf$AUCDEC, ENGdf$AUCDEC,HHdf$AUCDEC,MTHdf$AUCDEC,PHYdf$AUCDEC)
Plotdf=data.frame(BIOdf$AUCPCTDEC, ENGdf$AUCPCTDEC,HHdf$AUCPCTDEC,MTHdf$AUCPCTDEC,PHYdf$AUCPCTDEC)
Plotdf=-Plotdf
Plotdf[Plotdf<0]=0
Plotdf=data.frame(apply(Plotdf, 2, function(x){return(x/sum(x))}))
#Order=order(rowMeans(Plotdf), decreasing = TRUE)#reorder so most important come first
Plotdf=Plotdf[Order, ] 

Plotdf1=data.frame(cbind(c(rep(VarGroupNames[Order],5)),c(Plotdf[,1],Plotdf[,2],Plotdf[,3],Plotdf[,4],Plotdf[,5])))
names(Plotdf1)=c("Variable", "Importance")
Plotdf1$Variable=factor(Plotdf1$Variable, levels = VarGroupNames[rev(Order)])
Plotdf1$Major=rep(c("BIO", "ENG", "HH", "MTH", "PHY"), each=15)
Plotdf1$Importance=as.numeric(as.character((Plotdf1$Importance)))
p1 <- ggplot(Plotdf1, aes(Major, Variable)) + geom_tile(aes(fill = Importance),colour = "white") + scale_fill_gradient(low = "white",high = "darkred")+ggtitle("Predictiveness of STEM Retention")+theme(legend.position = "none")
p1

########################################################################################
#Plot used in paper
Plotdf=rbind(Plotdf2, Plotdf1)
Plotdf$Risk=c(rep("Predictiveness of Leaving Institution", 75), rep("Predictiveness of Leaving STEM", 75))

ggplot(Plotdf, aes(Major, Variable)) + geom_tile(aes(fill = Importance),colour = "white") + scale_fill_gradient(low = "white",high = "red")+theme(legend.position = "none")+facet_wrap(~Risk, ncol =2)+labs(x="Type of Major", y="Predictor")+theme(axis.text=element_text(size=12), axis.title =element_text(size=14))+ theme(strip.text.x = element_text(size = 12))


