####Leaving ISU


load("VIISU_7-2.Rdata")


VarGroupNames=c("High School Courses and Grades", "Demographics", "ISU Activities", "Scholarships", "Academic Skills/Behaviors*", "Social Integration*", "Satisfaction with Major*","Satisfaction with ISU*", "Math/Science Self Efficacy*", "Financial Concerns*", "Standardized Tests", "First Semester Classes", "First Semester Midterm Grades", "ACT Interest Survey", "Learning Community Participation")
Plotdf=data.frame(BIOdf$AUCDEC, ENGdf$AUCDEC,HHdf$AUCDEC,MTHdf$AUCDEC,PHYdf$AUCDEC)
#Plotdf=data.frame(BIOdf$AUCPCTDEC, ENGdf$AUCPCTDEC,HHdf$AUCPCTDEC,MTHdf$AUCPCTDEC,PHYdf$AUCPCTDEC)
Plotdf=-Plotdf
Plotdf[Plotdf<0]=0
Plotdf=data.frame(apply(Plotdf, 2, function(x){return(x/sum(x))}))
Order=order(rowMeans(Plotdf), decreasing = TRUE)#reorder so most important come first
Plotdf=Plotdf[Order, ] 

Plotdf2=data.frame(cbind(c(rep(VarGroupNames[Order],5)),c(Plotdf[,1],Plotdf[,2],Plotdf[,3],Plotdf[,4],Plotdf[,5])))
names(Plotdf2)=c("Variable", "Importance")
Plotdf2$Variable=factor(Plotdf2$Variable, levels = VarGroupNames[rev(Order)])
Plotdf2$Major=rep(c("BIO", "ENG", "HH", "MTH", "PHY"), each=15)
Plotdf2$Importance=as.numeric(as.character((Plotdf2$Importance)))
p2 <- ggplot(Plotdf2, aes(Major, Variable)) + geom_tile(aes(fill = Importance),colour = "white") + scale_fill_gradient(low = "white",high = "red")+ggtitle("Predictiveness of Leaving ISU")+theme(legend.position = "none")+ theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
p2

########Leaving STEM

load("VISTEM_7-2.Rdata")
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
#p1 <- ggplot(Plotdf1, aes(Major, Variable)) + geom_tile(aes(fill = Importance),colour = "white") + scale_fill_gradient(low = "white",high = "darkred")+ggtitle("Predictiveness of Leaving STEM")+theme(legend.position = "none")
p1 <- ggplot(Plotdf1, aes(Major, Variable)) + geom_tile(aes(fill = Importance),colour = "white") + scale_fill_gradient(low = "white",high = "red")+ggtitle("Predictiveness of Leaving STEM")+theme(legend.position = "none")
p1
