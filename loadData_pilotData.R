# Load the data file and create data values


# Read the data
tempData = read.delim('../Data/pilotData_19.08.15.txt')
# Remove the rows with 'NA's
tempData = na.omit(tempData)



# Set up the data object
# The independent variables
#ParticipantID <- tempData$ID
NParticipants = length(unique(tempData$ID))
OldID <- tempData$ID
ParticipantID <- array(0,length(tempData$ID))
for (index in 1:NParticipants){
  ParticipantID[OldID == unique(OldID)[index]] = index
}


QuestionID <- tempData$Qnumber
NQuestions = length(unique(QuestionID))

# Convert sex to -1 male and 1 female - check this
Sex <- tempData$SEX
Sex[Sex == 1] <- -0.5
Sex[Sex == 2] <- 0.5

# Convert conditions to the same, probably don't need ot do this
#Condition <- tempData$CONDITION
Condition1 = array(0,dim=length(tempData$CONDITION))
Condition1[tempData$CONDITION == 1] = 1
Condition2 = array(0,dim=length(tempData$CONDITION))
Condition2[tempData$CONDITION == 3] = 1


# Dependent variable
Correct <- tempData$CORRECT
NObservations <- length(Correct)

Age1 = array(0,dim=length(tempData$AGE))
Age1[tempData$AGE == 1] = 1
Age2 = array(0,dim=length(tempData$AGE))
Age2[tempData$AGE == 3] = 1

Education <- tempData$EDU
Stimulus <- tempData$STIMULUS -1
Confidence <- tempData$CONFIDENCE


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

jags.data = list('ParticipantID','NParticipants','QuestionID','NQuestions','Sex','Condition1','Condition2','NObservations','Stimulus','Age1', 'Age2','Education','Correct','Confidence')

# What params are we measuring
# Note - we do not appear to have to declare the random
# effect means here, which means they are not in the
# summary, which is nice. However, for the purposes of
# generating predictive samples, we need the effects
jags.params = c('b_intercept','b_sex','b_correct','b_confidence','b_cond1','b_cond2','b_sex_cond1','b_sex_cond2','b_age1','b_age2','b_education','b_sex_education','b_education_cond1','b_education_cond2','b_stimulus','b_sex_stimulus','sigma_p_abs','sigma_q_abs','logLike','b_participant','b_question')

# The initial values are declared as a function, this means that
# each chain can have different initial values, alternatively
# they can be declared as fixed values in a list object
jags.init = function(){
  list('b_intercept' =rnorm(1,0,10),
       'b_sex'=rnorm(1,0,10),
       'b_correct'=rnorm(1,0,10),
       'b_confidence'=rnorm(1,0,10),
       'b_cond1'=rnorm(1,0,10),
       'b_cond2' =rnorm(1,0,10),
       'b_sex_cond1' =rnorm(1,0,10),
       'b_sex_cond2'=rnorm(1,0,10),
       'b_age1' = rnorm(1,0,10),
       'b_age2' = rnorm(1,0,10),
       'b_education' =rnorm(1,0,10),
       'b_sex_education' =rnorm(1,0,10),
       'b_education_cond1' =rnorm(1,0,10),
       'b_education_cond2' =rnorm(1,0,10),
       'b_stimulus'=rnorm(1,0,10),
       'b_sex_stimulus'=rnorm(1,0,10),
       'sigma_p'=runif(1,0,10),
       'sigma_q'=runif(1,0,10),
       'b_participant'=rnorm(NParticipants), # one value per participant
       'b_question'=rnorm(NQuestions)) # one value per question
}