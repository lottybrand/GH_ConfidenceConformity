# We need rjags to reed and compile the jags model
# coda gives nice functions for handling chain summaries
# and plots
library(rjags)
library(R2jags)
library(coda)

# Read the data
tempData = read.delim('mancheckPilotU_C_B.txt')
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
Condition <- tempData$Condition
#Condition1 = array(0,dim=length(tempData$CONDITION))
#Condition1[tempData$CONDITION == 1] = 1
#Condition2 = array(0,dim=length(tempData$CONDITION))
#Condition2[tempData$CONDITION == 3] = 1


# Dependent variable
Remember <- tempData$RememberTask
NObservations <- length(Remember)

#Age1 = array(0,dim=length(tempData$AGEnum))
#Age1[tempData$AGEnum == 1] = 1
#Age2 = array(0,dim=length(tempData$AGEnum))
#Age2[tempData$AGEnum == 2] = 1
#Age3 = array(0,dim=length(tempData$AGEnum))
#Age3[tempData$AGEnum ==3] = 1

#Education <- tempData$EDU
#Stimuli <- tempData$Stimulus -1
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

jags.data = list('ParticipantID','Sex','Condition','NObservations','Remember')

# What params are we measuring
# Note - we do not appear to have to declare the random
# effect means here, which means they are not in the
# summary, which is nice. However, for the purposes of
# generating predictive samples, we need the effects
jags.params = c('b_sex','b_cond','b_sex_cond','logLike')

# The initial values are declared as a function, this means that
# each chain can have different initial values, alternatively
# they can be declared as fixed values in a list object
jags.init = function(){
  list('b_sex'=rnorm(1),
       'b_cond'=rnorm(1),
       'b_sex_cond'=rnorm(1)
)}

# Create the model object
# n.burnin - number of bunins

# n.thin - Thinning is used to reduce auto correlation, which seems
# to be a bit of an issue here, hence the high thinning of 40
# Diagnostic plots can be used to investigate the auto correlation
# in the coda package

# n.iter - If we want n.samples we need n.iter = n.thin*n.samples + n.burnin

# model.file - The model itself is in another file

# n.cluster - Specify the number of cores to use, default is the number
# of chains, but if you don't have the cores then specify
# the n.cluster value here
n.chains = 3
n.cluster = n.chains
my.model <- jags.parallel(data=jags.data,inits=jags.init,
                          parameters.to.save = jags.params,
                          model.file='rememberTaskModel.jags',
                          n.chains = n.chains,n.iter=61000,n.burnin=1000,
                          n.thin=20,n.cluster=n.cluster)

# This function returns coda compatible objects, for use with coda functions
my.samplesRememberTask <- as.mcmc(my.model)
save('my.samplesRememberTask',file =  'my.samplesRememberTask.dump')

# Lets focus on the relevant parameters (ignore participant and question pars)
relPars = c('b_cond','b_sex','b_sex_cond')

# View the parameter densities and trace
plot(my.samplesRememberTask[,relPars])

# View summary stats
my.summary = summary(my.samplesRememberTask[,relPars])
my.summary

# Odds ratio for sex condition
# the increase in probability of being
# fully confident given that you are male?
# Not sure about this
exp(my.summary$statistics[2,1])

#maximum loglikelihood
max(as.matrix(my.samplesRememberTask[,'logLike']))