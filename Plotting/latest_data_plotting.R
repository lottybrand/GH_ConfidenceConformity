library(reshape2)
library(MASS)
library(Hmisc)
library(ggplot2)

#organising data
qDataL = read.delim('../Data/Qualtrics_RAW_21.11.16.txt')
# Remove the rows with 'NA's
qDataL = na.omit(qDataL)

#to see what the colname numbers are for reshaping correctly
#m <- data.frame(matrix(0, ncol = 2, nrow = 127))
#m$X1 <- 1:127
#m$X2 <- names(l_data)
#m

qData <- reshape(qDataL, idvar = "ID", 
                 varying = list(c(8,13,18,23,28,33,38,43,48,53,58,63,68,73,78,83,88,93,98,103,108,113,118,123),
                                c(9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,99,104,109,114,119,124),
                                c(10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125),
                                c(11,16,21,26,31,36,41,46,51,56,61,66,71,76,81,86,91,96,101,106,111,116,121,126),
                                c(12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87,92,97,102,107,112,117,122,127)),
                 v.names = c("Response", "Confidence","pAnswer","pSaw","pFinal"), 
                 direction = "long")


colnames(qData)[8] <- "Qnumber"

# create these to create the Correct column below
correct1 <- c(1,4,7,9,10,12,14,16,18,19,21,23)                 
correct2 <- c(2,3,5,6,8,11,13,15,17,20,22,24)

qData$Correct <- ifelse((qData$Qnumber%in%correct1 & qData$Response==1),1,
                        ifelse((qData$Qnumber%in%correct2 & qData$Response==2),1,0))

qData$Switched <- ifelse(qData$Response==qData$pFinal, 0,1)

qData$majDisagreed <- ifelse((qData$pAnswer=="yes" & qData$pSaw>5),0,
                             ifelse((qData$pAnswer=="no" & qData$pSaw<5),0,1))

qData$noDisagreed <- ifelse(qData$pAnswer=="no", qData$pSaw, 12-qData$pSaw)

qData$CONDITION <- ifelse((qData$CondName=="Control Group"),2,
                          ifelse((qData$CondName=="Condition 1"),1,3))

qData$MajCorrect <- ifelse((qData$Correct==0 & qData$majDisagreed==1),1,
                           ifelse((qData$Correct==1 & qData$majDisagreed==0),1,0))

#line added 13.11.16 for prop_disagreed
qData$prop_disagreed <- qData$noDisagreed/12




levels(qData$CONDITION) <- c(levels(qData$CONDITION), "Females Better Prime", "Control", "Males Better Prime")
qData$CONDITION[qData$CONDITION==1]  <- "Females Better Prime"
qData$CONDITION[qData$CONDITION==2] <- "Control"
qData$CONDITION[qData$CONDITION==3] <- "Males Better Prime"


qData$Sex <- ifelse(qData$SEX==2, "Female", "Male")
Sex <- qData$Sex
as.factor(Sex)

Condition <- qData$CONDITION
as.factor(Condition)

Confidence <- qData$CONFIDENCE
Correct <- qData$CORRECT
Switching <-qData$Switched
Prop.Disagreed <- qData$prop_disagreed
noDisagreed <- qData$noDisagreed


##plotting conf (taken from plotting_18_10_16.R in pilotData folder)
confPlot <- ggplot(qData, aes(Condition, Confidence, color= Sex)) +
  stat_summary(fun.y=mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,6)) +
  scale_x_discrete(limits=c("Females Better Prime", "Control", "Males Better Prime"))
confPlot

#CorrectPlotting
correctPlot <- ggplot(qData, aes(Condition, Correct, color= Sex)) +
  stat_summary(fun.y=mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_discrete(limits=c("Females Better Prime", "Control", "Males Better Prime"))
correctPlot


#confidence & correct relationship

confCorrect <- ggplot(qData, aes(Confidence, Correct, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1))
confCorrect

#switching per conf plot
switchingPlot <- ggplot(qData, aes(Confidence, Switching, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) 
switchingPlot

#switching per prop disagreed
switchingPropPlot <- ggplot(qData, aes(Prop.Disagreed, Switching, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) 
switchingPropPlot

#switching per no.disagreed
switchingnoDisagreedPlot <- ggplot(qData, aes(noDisagreed, Switching, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.6) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_discrete(limits=c(0,2,4,8,10,12)) +
  xlab("Number Disagreed") + ylab("Proportion Switched")
switchingnoDisagreedPlot

#add stat_smooth?
SmoothSwitchingPropPlot <- ggplot(qData, aes(Prop.Disagreed, Switching, color= Sex)) +
  stat_summary(fun.y = mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  geom_smooth(method= "lm", se= FALSE,  colour= "red", formula=y ~ poly(x, 3, raw=TRUE)) +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) 
SmoothSwitchingPropPlot

#stat smooth per Conf? Can't seem to get it for all conf levels...
#try confidence as factor?
Confidence <- as.factor(Confidence) #nope doesn't work

#try for no.disagreed as factor
noDisagreed <- as.factor(noDisagreed)

SmoothConfPropPlot <- ggplot(qData, aes(Prop.Disagreed, Switching, colour= factor(Confidence))) +
  stat_summary(fun.y = mean, geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  geom_smooth(method= "lm", se= FALSE, formula=y ~ poly(x, 3, raw=TRUE)) +
  theme_bw()+
  scale_y_continuous(limits=c(0,1)) +
  scale_fill_discrete(name = "Confidence") +
  xlab("Proportion Disagreed") + ylab("Proportion Switched")
  
SmoothConfPropPlot


#density plot for confidence?

densityConf <- ggplot(qData, aes(Confidence, fill = Sex)) +
  geom_density(alpha = 0.2) + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_x_continuous(limits=c(0,6), expand= c(0,0)) +
  xlab("\nConfidence") + ylab("Density") 
densityConf

#density plot for switching
densitySwitch <- ggplot(qData, aes(Switching, fill = Sex)) +
  geom_density(alpha = 0.2) + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_x_continuous(limits=c(0,1), expand= c(0,0)) +
  xlab("\nSwitching") + ylab("Density") 
densitySwitch



#means etc

theMeansConf = tapply(qData$Confidence, list(qData$CONDITION, qData$Sex),mean)
theMeansConf

theMeansConfS = tapply(qData$Confidence, list(qData$Sex),mean)
theMeansConfS

theMeansCorrect = tapply(qData$Correct, list(qData$CONDITION, qData$Sex),mean)
theMeansCorrect

theMeansSwitching = tapply(qData$Switched, list(qData$CONDITION, qData$Sex),sum)
theMeansSwitching

table(Switching)


ggplot(melt(theMeansConf,varnames=c('condition','sex')),aes(y=value,x=as.factor(condition),colour=as.factor(sex))) + 
  geom_point(size=4)

sampleSize <- qData[!duplicated(qData$ID),]

ssCondition <- sampleSize$CONDITION
table(ssCondition)

FsampleSize <- sampleSize[(sampleSize$SEX==2),]
table(FsampleSize$CONDITION)
MsampleSize <- sampleSize[(sampleSize$SEX==1),]
table(MsampleSize$CONDITION)

####################CRITICAL TRIALS#############################
#create data of just critical trials and re-set names for plotting
qDataCritical <- qData[qData$majDisagreed==1,]

#critical trials and maj correct
critical_and_correct <- qDataCritical[qDataCritical$MajCorrect==1,]

#critical trials and maj incorrect
critical_and_INcorrect <- qDataCritical[qDataCritical$MajCorrect==0,]

table(qData$Switched)
table(qDataCritical$Switched)
table(critical_and_correct$Switched)
table(critical_and_INcorrect$Switched)


#table of social info breakdown
Trialtype <- c("All", "Critical","MajorityCorrect","MajorityIncorrect")  
totalTrials <- c(3120, 1524, 706, 818)
totalSwitched <- c(462, 428, 241, 187)
PropSwitched <- c(0.15, 0.28, 0.34, 0.23)
SocInfo <- (data.frame(Trialtype, totalTrials, totalSwitched, PropSwitched))



#trying an ordinal logistic model from the web

#ordlogist<-polr(Confidence ~ Sex + Condition, data=qData,Hess=T)
#summary(ordlogist)

summary(qData$CONDITION)
table(Condition)
table(Confidence)
