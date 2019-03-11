#survey answers History
library("ggthemes")
library("cowplot")
library("NLP")
library("tm")
library("RColorBrewer")
library("wordcloud")
library("ggplot2")

setwd("**************")
qdf=read.csv("hidtorical survey questions.csv",header=F)
qarr=as.vector(qdf[1:52,4])

#2018 Survey
s18=read.csv("engagement 2018 holistic dat.csv",header=T,stringsAsFactors = FALSE)
s18=s18[-1,]
cnames=c()
for (i in c(1:NCOL(s18))){cnames=c(cnames,as.String(s18[1,i]))}
colnames(s18)=(cnames)
s18=s18[-1,]

#2017 Survey
s17=read.csv("engagement 2017 holistic dat.csv",header=F,stringsAsFactors = FALSE)
s17=s17[-1:-2,]
cnames=c()
for (i in c(1:NCOL(s17))){cnames=c(cnames,as.String(s17[1,i]))}
colnames(s17)=(cnames)
s17=s17[-1,]

#2016 Survey
s16=read.csv("engagement 2016 holistic dat.csv",header=F,stringsAsFactors = FALSE)
s16=s16[-1:-2,]
cnames=c()
for (i in c(1:NCOL(s16))){cnames=c(cnames,as.String(s16[1,i]))}
colnames(s16)=(cnames)
s16=s16[-1,]

#2015 Survey
s15=read.csv("engagement 2015 holistic dat.csv",header=F,stringsAsFactors = FALSE)
s15=s15[-1:-2,]
cnames=c()
for (i in c(1:NCOL(s15))){cnames=c(cnames,as.String(s15[1,i]))}
colnames(s15)=(cnames)
s15=s15[-1,]

cs18=s18[s18[,2]%in%qarr,]
cs17=s17[s17[,2]%in%qarr,]
cs16=s16[s16[,2]%in%qarr,]
cs15=s15[s15[,2]%in%qarr,]

newdf=data.frame()
newdf=rbind(newdf,cs18)
newdf=rbind(newdf,cs17)
newdf=rbind(newdf,cs16)
newdf=rbind(newdf,cs15)
newdf=newdf[order(newdf$Item),]
newdf=newdf[ order(newdf$Scale),]


c(2018,2017,2016,2015)
fdf=newdf[,1:4]
str(fdf)
fdf$`Avg (Raw)` <- as.numeric(as.character(fdf$`Avg (Raw)`))
#fdf$Item=as.factor(fdf$Item)
fdf[,4]=rep(c(2018,2017,2016,2015),nrow(fdf)/4)

#Accountability----------------------------------------------------------------------------------------------------------------------
fdf1=fdf[fdf$Scale=="Accountability",]
a=ggplot(fdf1, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg <- get_legend(a)
a1=ggplot(fdf1, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a1=a1+labs(x="Survey Year",y="Score Result",title="Accountability Questions 1-3")
plot(leg)
plot(a1)
ggsave("a1.png")

#Communication-----------------------------------------------------------------------------------------------------------------------
fdf2=fdf[fdf$Scale=="Communication",]
ab=ggplot(fdf2, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg2 <- get_legend(ab)
a2=ggplot(fdf2, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a2=a2+labs(x="Survey Year",y="Score Result",title="Communication Questions 4-7")
plot(leg2)
plot(a2)
ggsave("a2.png")

#Empowerment/ Autonomy---------------------------------------------------------------------------------------------------------------
fdf3=fdf[fdf$Scale=="Empowerment/ Autonomy",]
ac=ggplot(fdf3, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg3 <- get_legend(ac)
a3=ggplot(fdf3, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a3=a3+labs(x="Survey Year",y="Score Result",title="Empowerment/ Autonomy Questions 8 and 9")
plot(leg3)
plot(a3)
ggsave("a3.png")

#Execution---------------------------------------------------------------------------------------------------------------------------
fdf4=fdf[fdf$Scale=="Execution",]
ad=ggplot(fdf4, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg4 <- get_legend(ad)
a4=ggplot(fdf4, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a4=a4+labs(x="Survey Year",y="Score Result",title="Execution Questions 10-12")
plot(leg4)
plot(a4)
ggsave("a4.png")

#Innovation---------------------------------------------------------------------------------------------------------------------------
fdf5=fdf[fdf$Scale=="Innovation",]
ae=ggplot(fdf5, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg5 <- get_legend(ae)
a5=ggplot(fdf5, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a5=a5+labs(x="Survey Year",y="Score Result",title="Innovation Question 13")
plot(leg5)
plot(a5)
ggsave("a5.png")

#Leadership and Motivation-------------------------------------------------------------------------------------------------------------
fdf6=fdf[fdf$Scale=="Leadership and Motivation",]
af=ggplot(fdf6, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg6 <- get_legend(af)
a6=ggplot(fdf6, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(2,5))
a6=a6+labs(x="Survey Year",y="Score Result",title="Leadership and Motivation Questions 14-18")
plot(leg6)
plot(a6)
ggsave("a6.png")

#Management Support---------------------------------------------------------------------------------------------------------------------
fdf7=fdf[fdf$Scale=="Management Support",]
ag=ggplot(fdf7, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg7 <- get_legend(ag)
a7=ggplot(fdf7, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a7=a7+labs(x="Survey Year",y="Score Result",title="Management Support Question 19")
plot(leg7)
plot(a7)
ggsave("a7.png")

#Opportunities for Growth---------------------------------------------------------------------------------------------------------------------
fdf8=fdf[fdf$Scale=="Opportunities for Growth",]
ah=ggplot(fdf8, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg8 <- get_legend(ah)
a8=ggplot(fdf8, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a8=a8+labs(x="Survey Year",y="Score Result",title="Opportunities for Growth Questions 20-21")
plot(leg8)
plot(a8)
ggsave("a8.png")

#Organizational Effectiveness---------------------------------------------------------------------------------------------------------------------
fdf9=fdf[fdf$Scale=="Organizational Effectiveness",]
ai=ggplot(fdf9, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg9 <- get_legend(ai)
a9=ggplot(fdf9, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a9=a9+labs(x="Survey Year",y="Score Result",title="Organizational Effectiveness Question 22")
plot(leg9)
plot(a9)
ggsave("a9.png")

#Personal Expression---------------------------------------------------------------------------------------------------------------------
fdf10=fdf[fdf$Scale=="Personal Expression",]
aj=ggplot(fdf10, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg10 <- get_legend(aj)
a10=ggplot(fdf10, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a10=a10+labs(x="Survey Year",y="Score Result",title="Personal Expression Questions 23-24")
plot(leg10)
plot(a10)
ggsave("a10.png")

#PO-Communication---------------------------------------------------------------------------------------------------------------------
fdf11=fdf[fdf$Scale=="PO-Communication",]
ak=ggplot(fdf11, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg11 <- get_legend(ak)
a11=ggplot(fdf11, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(3,4.5))
a11=a11+labs(x="Survey Year",y="Score Result",title="PO-Communication Questions 25-27")
plot(leg11)
plot(a11)
ggsave("a11.png")

#PO-Management Support---------------------------------------------------------------------------------------------------------------------
fdf12=fdf[fdf$Scale=="PO-Management Support",]
al=ggplot(fdf12, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg12 <- get_legend(al)
a12=ggplot(fdf12, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a12=a12+labs(x="Survey Year",y="Score Result",title="PO-Management Support Questions 28-29")
plot(leg12)
plot(a12)
ggsave("a12.png")

#PO-Organizational Effectiveness---------------------------------------------------------------------------------------------------------------------
fdf13=fdf[fdf$Scale=="PO-Organizational Effectiveness",]
am=ggplot(fdf13, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg13 <- get_legend(am)
a13=ggplot(fdf13, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a13=a13+labs(x="Survey Year",y="Score Result",title="PO-Organizational Effectiveness Question 30")
plot(leg13)
plot(a13)
ggsave("a13.png")

#Purpose and Direction---------------------------------------------------------------------------------------------------------------------
fdf14=fdf[fdf$Scale=="Purpose and Direction",]
an=ggplot(fdf14, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg14 <- get_legend(an)
a14=ggplot(fdf14, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(3.0,5))
a14=a14+labs(x="Survey Year",y="Score Result",title="Purpose and Direction Questions 31-35")
plot(leg14)
plot(a14)
ggsave("a14.png")

#Quality and Customer Focus---------------------------------------------------------------------------------------------------------------------
fdf15=fdf[fdf$Scale=="Quality and Customer Focus",]
ao=ggplot(fdf15, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg15 <- get_legend(ao)
a15=ggplot(fdf15, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a15=a15+labs(x="Survey Year",y="Score Result",title="Quality and Customer Focus Question 36")
plot(leg15)
plot(a15)
ggsave("a15.png")

#Recognition---------------------------------------------------------------------------------------------------------------------
fdf16=fdf[fdf$Scale=="Recognition",]
ap=ggplot(fdf16, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg16 <- get_legend(ap)
a16=ggplot(fdf16, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a16=a16+labs(x="Survey Year",y="Score Result",title="Recognition Question 37")
plot(leg16)
plot(a16)
ggsave("a16.png")

#Respect for Employees---------------------------------------------------------------------------------------------------------------------
fdf17=fdf[fdf$Scale=="Respect for Employees",]
aq=ggplot(fdf17, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg17 <- get_legend(aq)
a17=ggplot(fdf17, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(2,5))
a17=a17+labs(x="Survey Year",y="Score Result",title="Respect for Employees Questions 38 and 39")
plot(leg17)
plot(a17)
ggsave("a17.png")

#Respect for Management---------------------------------------------------------------------------------------------------------------------
fdf18=fdf[fdf$Scale=="Respect for Management",]
ar=ggplot(fdf18, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg18 <- get_legend(ar)
a18=ggplot(fdf18, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a18=a18+labs(x="Survey Year",y="Score Result",title="Respect for Management Questions 40 and 41")
plot(leg18)
plot(a18)
ggsave("a18.png")

#Stress and Workload---------------------------------------------------------------------------------------------------------------------
fdf19=fdf[fdf$Scale=="Stress and Workload",]
as=ggplot(fdf19, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg19 <- get_legend(as)
a19=ggplot(fdf19, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a19=a19+labs(x="Survey Year",y="Score Result",title="Stress and Workload Questions 42 and 43")
plot(leg19)
plot(a19)
ggsave("a19.png")

#Teamwork and Cooperation---------------------------------------------------------------------------------------------------------------------
fdf20=fdf[fdf$Scale=="Teamwork and Cooperation",]
at=ggplot(fdf20, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg20 <- get_legend(at)
a20=ggplot(fdf20, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a20=a20+labs(x="Survey Year",y="Score Result",title="Teamwork and Cooperation Questions 44 and 45")
plot(leg20)
plot(a20)
ggsave("a20.png")

#Trust---------------------------------------------------------------------------------------------------------------------
fdf21=fdf[fdf$Scale=="Trust",]
au=ggplot(fdf21, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg21 <- get_legend(au)
a21=ggplot(fdf21, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a21=a21+labs(x="Survey Year",y="Score Result",title="Trust Questions 46-48")
plot(leg21)
plot(a21)
ggsave("a21.png")

#Values---------------------------------------------------------------------------------------------------------------------
fdf22=fdf[fdf$Scale=="Values",]
av=ggplot(fdf22, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg22 <- get_legend(av)
a22=ggplot(fdf22, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a22=a22+labs(x="Survey Year",y="Score Result",title="Values Questions 49 and 51")
plot(leg22)
plot(a22)
ggsave("a22.png")

#Workplace and Resources---------------------------------------------------------------------------------------------------------------------
fdf23=fdf[fdf$Scale=="Workplace and Resources",]
aw=ggplot(fdf23, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()
leg23 <- get_legend(aw)
a23=ggplot(fdf23, aes(x=as.numeric(Percentile), y=`Avg (Raw)`,col = (Item))) + geom_line()+theme_pander()+theme(legend.position="none")+scale_y_continuous(limits = c(1,5))
a23=a23+labs(x="Survey Year",y="Score Result",title="Workplace and Resources Question 52")
plot(leg23)
plot(a23)
ggsave("a23.png")
