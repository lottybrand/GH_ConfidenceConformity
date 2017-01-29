# Load the data file and create data values


# Read the data
tempData = read.delim('manCheckPilot.txt')
# Remove the rows with 'NA's
#tempData = na.omit(tempData)

# Set up the data object
# The independent variables
ParticipantID <- tempData$ID
#NParticipants = length(unique(tempData$ID))
#OldID <- tempData$ID
#ParticipantID <- array(0,length(tempData$ID))
#for (index in 1:NParticipants){
#  ParticipantID[OldID == unique(OldID)[index]] = index
#}



#QuestionID <- tempData$Qnumber
#NQuestions = length(unique(QuestionID))

# Convert sex to -1 male and 1 female - check this
Sex <- tempData$Sex
Sex[Sex == 1] <- -0.5
Sex[Sex == 2] <- 0.5

# Convert conditions to the same, probably don't need ot do this
#Condition <- tempData$Condition
Condition1 = array(0,dim=length(tempData$Condition))
Condition1[tempData$Condition == 1] = 1
Condition2 = array(0,dim=length(tempData$Condition))
Condition2[tempData$Condition == 3] = 1


# Dependent variable
ExpOpp <- tempData$ExpectOPP
#Correct <- tempData$CORRECT
NObservations <- length(ExpOpp)

#Age1 = array(0,dim=length(tempData$AGE))
#Age1[tempData$AGE == 1] = 1
#Age2 = array(0,dim=length(tempData$AGE))
#Age2[tempData$AGE == 2] = 1
#Age3 = array(0,dim=length(tempData$AGE))
#Age3[tempData$AGE ==3] = 1

#Education <- tempData$EDU
#Stimulus <- tempData$Stimulus -1
#Confidence <- tempData$CONFIDENCE


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

jags.data = list('ParticipantID','NObservations','Sex','Condition1','Condition2','ExpOpp')

# What params are we measuring
# Note - we do not appear to have to declare the random
# effect means here, which means they are not in the
# summary, which is nice. However, for the purposes of
# generating predictive samples, we need the effects
jags.params = c('b_sex','b_cond1','b_cond2','b_sex_cond1','b_sex_cond2')

# The initial values are declared as a function, this means that
# each chain can have different initial values, alternatively
# they can be declared as fixed values in a list object
jags.init = function(){
  list('b_sex'=rnorm(1),
       'b_cond1'=rnorm(1),
       'b_cond2'=rnorm(1),
       'b_sex_cond1'=rnorm(1),
       'b_sex_cond2'=rnorm(1)) # one value per question
}