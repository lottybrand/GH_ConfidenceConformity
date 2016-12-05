#graphs for chapter one, confidence & conformity, 18-10-16

#first pilot data:

#from pilotData folder
source("../loadData_pilotData.R")
library(MASS)
library(Hmisc)
library(ggplot2)

describe(tempData)
summary(tempData)

levels(tempData$CONDITION) <- c(levels(tempData$CONDITION), "Control", "Males Better Prime", "Females Better Prime")
tempData$CONDITION[tempData$CONDITION==1]  <- "Females Better Prime"
tempData$CONDITION[tempData$CONDITION==2] <- "Control"
tempData$CONDITION[tempData$CONDITION==3] <- "Males Better Prime"


tempData$Sex <- ifelse(tempData$SEX==2, "Female", "Male")
Sex <- tempData$Sex
as.factor(Sex)

Condition <- tempData$CONDITION
Confidence <- tempData$CONFIDENCE
Correct <- tempData$CORRECT

#unnecessary subsetting  
#Cond1Sex1 <- tempData[tempData$CONDITION==1 & tempData$SEX==1,]
#Cond1Sex2 <- tempData[tempData$CONDITION==1 & tempData$SEX==2,]
#Cond2Sex1 <- tempData[tempData$CONDITION==2 & tempData$SEX==1,]
#Cond3Sex1 <- tempData[tempData$CONDITION==3 & tempData$SEX==1,]
#Cond2Sex2 <- tempData[tempData$CONDITION==2 & tempData$SEX==2,]
#Cond3Sex2 <- tempData[tempData$CONDITION==3 & tempData$SEX==2,]

#summary(Cond1Sex1)

#ordinal logistic model 
#ordlogist<-polr(Confidence ~ Sex + Condition1 + Condition2, data=tempData,Hess=T)
#summary(ordlogist)

#confidence point plot (maybe try boxplot?? medians all the same??)
confPlot <- ggplot(tempData, aes(Condition, Confidence, color= Sex)) +
  stat_summary(fun.y=mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,6)) +
  scale_x_discrete(limits=c("Females Better Prime", "Control", "Males Better Prime"))
confPlot

#correct plot 

correctPlot <- ggplot(tempData, aes(Condition, Correct, color= Sex)) +
  stat_summary(fun.y=mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_discrete(limits=c("Females Better Prime", "Control", "Males Better Prime"))
correctPlot

#confidence & correct relationship

confCorrect <- ggplot(tempData, aes(Confidence, Correct, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1))
confCorrect

######################################################################################
### ************** now for conformity data: *******************************
### ************LOAD BODY GAME SOCIAL FILE BEFORE RUNNING BELOW ********** 
### *********** ******************************************************************
###################################################################################### 

#from loadData_bgSocial.... run file below directly instead
#source("../loadData_bgSocial.R")

#line added 13.11.16 for prop_disagreed
tempData$prop_disagreed <- tempData$noDisagreed/12
Prop_D <- tempData$prop_disagreed

tempData$Condition <- ifelse(tempData$Condition==1, "Females Better", "Males Better")
Condition <- tempData$Condition  
as.factor(Condition)

tempData$Sex <- ifelse(tempData$Sex==2, "Female", "Male")
Sex <- tempData$Sex
as.factor(Sex)

#confidence point plot (maybe try boxplot?? medians all the same??)
conf2Plot <- ggplot(tempData, aes(Condition, Confidence, color= Sex)) +
  stat_summary(fun.y=mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,6)) +
  scale_x_discrete(limits=c("Females Better", "Males Better"))
conf2Plot

#correct plot 

correct2Plot <- ggplot(tempData, aes(Condition, Correct, color= Sex)) +
  stat_summary(fun.y=mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_discrete(limits=c("Females Better", "Males Better"))
correct2Plot

#switching per confidence plot? 

switchingPlot <- ggplot(tempData, aes(Confidence, Switching, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) 
switchingPlot

#confidence & correct relationship

confCorrect2 <- ggplot(tempData, aes(Confidence, Correct, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1))
confCorrect2

#density plot for confidence?

densityConf <- ggplot(tempData, aes(Confidence, fill = Sex)) +
  geom_density(alpha = 0.2) + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_x_continuous(limits=c(0,6), expand= c(0,0)) +
  xlab("\nConfidence") + ylab("Density") 
densityConf

#density plot for switching
densitySwitch <- ggplot(tempData, aes(Switching, fill = Sex)) +
  geom_density(alpha = 0.2) + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_x_continuous(limits=c(0,1), expand= c(0,0)) +
  xlab("\nSwitching") + ylab("Density") 
densitySwitch

###############Looking at Critical Trials ################
#playing with data for "critical trials" 
#switched_and_Critical <- ifelse(tempData$MajDiffered==1 & tempData$Switched ==1, 1, 0)
#table(switched_and_Critical)

#create data of just critical trials and re-set names for plotting
tempDataCritical <- tempData[tempData$MajDiffered==1,]
CriticalSwitched <- tempDataCritical$Switched
Sex <- tempDataCritical$Sex
Confidence <- tempDataCritical$Confidence
table(CriticalSwitched)
table(Switching)

#critical trials and maj correct
critical_and_correct <- tempDataCritical[tempDataCritical$MajCorrect==1,]
table(critical_and_correct$Switched)

#critical trials and maj incorrect
critical_and_INcorrect <- tempDataCritical[tempDataCritical$MajCorrect==0,]
table(critical_and_INcorrect$Switched)

#table of social info breakdown
Trialtype <- c("All", "Critical","MajorityCorrect" )  
totalTrials <- c(2419, 1138, 260)
totalSwitched <- c(204, 194, 75)
PropSwitched <- c(0.08, 0.17, 0.29)
SocInfo <- (data.frame(Trialtype, totalTrials, totalSwitched, PropSwitched))


  
#density plot for switching critical 
densitySwitchCritical <- ggplot(tempDataCritical, aes(Switched, fill = Sex)) +
  geom_density(alpha = 0.2) + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_x_continuous(limits=c(0,1), expand= c(0,0)) +
  xlab("\nSwitching") + ylab("Density") 
densitySwitchCritical

#switching per confidence plot for Critical Trials 

switchingPlotCritical <- ggplot(tempDataCritical, aes(Confidence, Switched, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) 
switchingPlotCritical

#switching per prop disagreed
switchingPropPlot <- ggplot(qData, aes(Prop.Disagreed, Switching, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) 
switchingPropPlot

#switching per no.disagreed
switchingnoDisagreedPlot <- ggplot(tempData, aes(noDisagreed, Switching, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.6) +
  theme_bw() +
  scale_x_discrete(limits = c(0,2,4,8,10,12)) +
  scale_y_continuous(limits=c(0,1)) +
  xlab("Number Disagreed") + ylab("Proportion Switched")
switchingnoDisagreedPlot


#spaceships plot for comparison
#limits <- aes(ymax = 1-d.pred$PI.U, ymin = 1-d.pred$PI.L)
#tryingPlot <- ggplot(data = d.pred, aes(Condition, 1-means, shape = Sex))
#tryingPlot + geom_point(data = d.pred, stat="identity", position = position_dodge(width=0.3), size = 3.5) + 
#  geom_errorbar(limits, width = 0.08, position = position_dodge(width=0.3)) +
#  geom_hline(aes(yintercept=0.5), linetype="dashed", show.legend=FALSE) + 
#  theme_bw() + theme(text = element_text(size=12), axis.title.x=element_blank(), axis.title.y=element_text(margin=margin(0,12,0,0))) + 
#  ylab("Proportion Chose Social Information") +
#  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
#  scale_x_discrete(limits=c("Control", "Social Risky","Asocial Risky"))
