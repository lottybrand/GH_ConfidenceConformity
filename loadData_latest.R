# Load the data file and create data values


# Read the data
qDataL = read.delim('Qualtrics_RAW_21.11.16.txt')
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

# Convert conditions to the same, probably don't need ot do this
#Condition <- qData$CONDITION
Condition1 = array(0,dim=length(qData$CONDITION))
Condition1[qData$CONDITION == 1] = 1
Condition2 = array(0,dim=length(qData$CONDITION))
Condition2[qData$CONDITION == 3] = 1


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

# Declare the data as a named list
# Names need to match the model file
#jags.data = list('ParticipantID',
#'NParticipants',
#'QuestionID',
#'NQuestions',
#'Sex',
#'Condition',
#'Correct',
#'NObservations')

jags.data = list('ParticipantID','NParticipants','QuestionID','NQuestions','Sex','Condition1','Condition2','NObservations','Correct','Confidence','noDisagreed','Switching')

# What params are we measuring
# Note - we do not appear to have to declare the random
# effect means here, which means they are not in the
# summary, which is nice. However, for the purposes of
# generating predictive samples, we need the effects
jags.params = c('b_intercept','b_sex','b_correct','b_confidence','b_sex_confidence','b_noDisagreed','b_cond1','b_cond2','b_sex_cond1','b_sex_cond2','sigma_p_abs','sigma_q_abs','logLike','b_participant','b_question')

# The initial values are declared as a function, this means that
# each chain can have different initial values, alternatively
# they can be declared as fixed values in a list object
jags.init = function(){
  list('b_intercept' =rnorm(1),
       'b_sex'=rnorm(1),
       'b_correct'=rnorm(1),
       'b_cond1'=rnorm(1),
       'b_cond2'=rnorm(1),
       'b_sex_cond1'=rnorm(1),
       'b_sex_cond2'=rnorm(1),
       'b_confidence'=rnorm(1),
       'b_sex_confidence'=rnorm(1),
       'b_noDisagreed'=rnorm(1),
       'sigma_p'=runif(1),
       'sigma_q'=runif(1),
       'b_participant'=rnorm(NParticipants), # one value per participant
       'b_question'=rnorm(NQuestions)) # one value per question
}