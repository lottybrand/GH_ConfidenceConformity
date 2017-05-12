#trying out ordered logit models for latest Q data, Chapter 3 of thesis. 

library(rethinking)

# prepare data using relevant lines from loadData_latest.R

qDataL = read.delim('../Data/Qualtrics_RAW_21.11.16.txt')
# Remove the rows with 'NA's
qDataL = na.omit(qDataL)


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

#line added 13.11.16 for prop_disagreed
qData$prop_disagreed <- qData$noDisagreed/12

#for testing with just critical trials:
qDataCritical <- qData[qData$majDisagreed==1,]
qData <- qDataCritical

# Set up the data object
# The independent variables
#ParticipantID <- tempData$ID
NParticipants = length(unique(qData$ID))
OldID <- qData$ID
ParticipantID <- array(0,length(qData$ID))
for (index in 1:NParticipants){
  ParticipantID[OldID == unique(OldID)[index]] = index
}


QuestionID <- qData$Qnumber
NQuestions = length(unique(QuestionID))

# Convert sex to -1 male and 1 female - check this
Sex <- qData$SEX
Sex[Sex == 1] <- -0.5
Sex[Sex == 2] <- 0.5
qData$Sex <- Sex

# Convert conditions to the same, probably don't need ot do this
#Condition <- qData$CONDITION
Condition1 = array(0,dim=length(qData$CONDITION))
Condition1[qData$CONDITION == 1] = 1
Condition2 = array(0,dim=length(qData$CONDITION))
Condition2[qData$CONDITION == 3] = 1

qData$Condition1 <- Condition1
qData$Condition2 <- Condition2

# Dependent variable
Correct <- qData$Correct
NObservations <- length(Correct)
Switching <- qData$Switched

#Age1 = array(0,dim=length(tempData$AGE))
#Age1[tempData$AGE == 1] = 1
#Age2 = array(0,dim=length(tempData$AGE))
#Age2[tempData$AGE == 3] = 1

#Education <- tempData$EDU
#Stimulus <- tempData$STIMULUS -1

Confidence <- qData$Confidence
noDisagreed <- qData$noDisagreed

##########################################

simplehist(qData$Confidence, xlim=c(0,6), xlab="confidence")

#make confidence 1-7 rather than 0-6 as I think this causes problems further down the line...
qData$Confidence <- qData$Confidence + 1

simplehist(qData$Confidence, xlim=c(1,7), xlab="confidence")

#R code 11.3 from rethinking:

pr_k <- table(qData$Confidence)/nrow(qData)

cum_pr_k <- cumsum(pr_k)

plot(1:7, cum_pr_k, type="b", xlab="confidence", ylab="cumulative proportion", ylim=c(0,1))

logit <- function(x) log(x/(1-x)) 
( lco <- logit (cum_pr_k) )

#basic model with no predictors

m11.1 <- map(
  alist(
    Confidence ~ dordlogit (phi, c(a1, a2, a3, a4, a5, a6)),
    phi <- 0, 
    c(a1,a2,a3,a4,a5,a6) ~ dnorm(0,10)
  ), 
  data=qData,
  start=list(a1=-2,a2=-1,a3=0,a4=1,a5=2,a6=2.5))

precis(m11.1)

logistic(coef(m11.1))

m11.1stan <- map2stan(
  alist(
    Confidence ~ dordlogit (phi, cutpoints),
    phi <- 0, 
    cutpoints ~ dnorm(0,10)
  ), 
  data=list(Confidence=qData$Confidence),
  start=list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains=2, cores=2)

precis(m11.1stan, depth=2)

m11.2 <- map(
  alist(
    Confidence ~ dordlogit(phi, c(a1,a2,a3,a4,a5,a6)),
    phi <- bs*Sex + bc1*Condition1 + bc2*Condition2,
    c(bs,bc1,bc2) ~ dnorm(0,10),
    c(a1,a2,a3,a4,a5,a6) ~ dnorm(0,10)
  ),
  data=qData,
  start=list(a1=-1.9, a2=-1.2, a3=-0.7, a4=0.2, a5=0.9, a6=1.8))

coeftab(m11.2)
precis(m11.2, depth=2)

compare(m11.1,m11.2)

m11.3 <- map(
  alist(
    Confidence ~ dordlogit(phi, c(a1,a2,a3,a4,a5,a6)),
    phi <- bs*Sex + bc1*Condition1 + bc2*Condition2 + bp*Correct + bsc1*Sex*Condition1 + bsc2*Sex*Condition2,
    c(bs,bc1,bc2,bp,bsc1,bsc2) ~ dnorm(0,10),
    c(a1,a2,a3,a4,a5,a6) ~ dnorm(0,10)
  ),
  data=qData,
  start=list(a1=-1.9, a2=-1.2, a3=-0.7, a4=0.2, a5=0.9, a6=1.8))

precis(m11.3,depth=2)

#try it in stan, to eventually try a multi-level?
m11.3stan <- map2stan(
  alist(
    Confidence ~ dordlogit (phi, cutpoints),
    phi <- bs*Sex + bc1*Condition1 + bc2*Condition2,
    cutpoints ~ dnorm(0,10),
    c(bs,bc1,bc2) ~ dnorm(0,10),
  ), 
  data=list(Confidence =qData$Confidence, Sex =qData$Sex, Condition1 = qData$Condition1, Condition2=qData$Condition2),
  start=list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains=2, cores=2)

