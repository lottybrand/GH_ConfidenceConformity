

# Read the data
tempData = read.delim('./Data/bodyGameSocial.txt')
# Remove the rows with 'NA's
tempData = na.omit(tempData)


# Set up the data object
# The independent variables


#Fancy messing with ppt ordering
#ParticipantID <- tempData$ID
NParticipants = length(unique(tempData$ID))
OldID <- tempData$ID
ParticipantID <- array(0,length(tempData$ID))
for (index in 1:NParticipants){
  ParticipantID[OldID == unique(OldID)[index]] = index
}


QuestionID <- tempData$QuestionNo
NQuestions = length(unique(QuestionID))
Correct <- tempData$Correct
Confidence <- tempData$Confidence
#Disagreed <- tempData$MajDiffered
NoDisagreed <- tempData$noDisagreed
#propDisagreed? 

# Convert sex to -1 male and 1 female - check this
Sex <- tempData$Sex
Sex[Sex == 1] <- -0.5
Sex[Sex == 2] <- 0.5

# Convert conditions to the same, probably don't need ot do this
Condition <- tempData$Condition
Condition[Condition == 1] <- -0.5
Condition[Condition == 2] <- 0.5

# Dependent variable
Switching <- tempData$Switched
NObservations <- length(Switching)



jags.data = list('ParticipantID','NParticipants','QuestionID','NQuestions','Sex','Correct','Condition','Confidence','NObservations','NoDisagreed','Switching')

# What params are we measuring
# Note - we do not appear to have to declare the random
# effect means here, which means they are not in the
# summary, which is nice. However, for the purposes of
# generating predictive samples, we need the effects
jags.params = c('b_intercept','b_sex','b_cond','b_conf','b_correct','b_noDisagreed','b_sex_cond','b_sex_conf','sigma_p_abs','sigma_q_abs','b_participant','b_question','logLike')

# The initial values are declared as a function, this means that
# each chain can have different initial values, alternatively
# they can be declared as fixed values in a list object
jags.init = function(){
  list('b_intercept'=rnorm(1,0,10),
       'b_sex'=rnorm(1,0,10),
       'b_cond'=rnorm(1,0,10),
       'b_conf'=rnorm(1,0,10),
       'b_correct' =rnorm(1,0,10),
       'b_noDisagreed' =rnorm(1,0,10),
       'b_sex_cond'=rnorm(1,0,10),
       'b_sex_conf' =rnorm(1,0,10),
       'sigma_p'=runif(1,0,10),
       'sigma_q'=runif(1,0,10),
       'b_participant'=rnorm(NParticipants),
       'b_question'=rnorm(NQuestions))
}