# We need rjags to reed and compile the jags model
# coda gives nice functions for handling chain summaries
# and plots
library(rjags)
library(coda)
library(R2jags)

# Read the data
#tempData = read.delim('../Data/bodyGameSocial.txt')
# Remove the rows with 'NA's
tempData <- qData
#tempData = na.omit(tempData)


# Set up the data object
# The independent variables
ParticipantID <- tempData$ID
NParticipants = length(unique(ParticipantID))
QuestionID <- tempData$QuestionNo
NQuestions = length(unique(QuestionID))

NParticipants = length(unique(tempData$ID))
OldID <- tempData$ID
ParticipantID <- array(0,length(tempData$ID))
for (index in 1:NParticipants){
  ParticipantID[OldID == unique(OldID)[index]] = index
}

Confidence <- tempData$Confidence
NoDisagreed <- tempData$noDisagreed
Correct <- tempData$Correct

# Convert sex to -1 male and 1 female - check this
Sex <- tempData$Sex
Sex[Sex == 1] <- -0.5
Sex[Sex == 2] <- 0.5


# Convert conditions to the same, probably don't need ot do this
Condition <- tempData$Condition
Condition[Condition == 1] <- -0.5
Condition[Condition == 2] <- 0.5

#Condition1 = array(0,dim=length(tempData$CONDITION))
#Condition1[tempData$CONDITION == 1] = 1
#Condition2 = array(0,dim=length(tempData$CONDITION))
#Condition2[tempData$CONDITION == 3] = 1


# Dependent variable

Switching <- tempData$Switched
NObservations <- length(Switching)


# Declare the data as a named list
# Names need to match the model file
jags.data = list('ParticipantID','NParticipants','QuestionID','NQuestions','Sex','Condition','NObservations','Confidence','NoDisagreed','Correct','Switching')


# What params are we measuring
# Note - we do not appear to have to declare the random
# effect means here, which means they are not in the
# summary, which is nice. However, for the purposes of
# generating predictive samples, we need the effects
jags.params = c('b_Cintercept','b_Ccorrect','b_CSex','b_CCond','sigma_CP_abs','sigma_CQ_abs','logLike',
                'b_CParticipant','b_CQuestion','b_Swintercept','b_SwSex','b_SwSex',
                'sigma_SwP_abs','sigma_SwQ_abs','b_SwParticipant','b_SwQuestion','b_SwNoDisagreed', 
                'b_SwConf','theta')

# The initial values are declared as a function, this means that
# each chain can have different initial values, alternatively
# they can be declared as fixed values in a list object
jags.init = function(){
  list('b_Cintercept'=rnorm(1),
       'b_CSex'=rnorm(1),
       'b_CCond'=rnorm(1),
       'b_Ccorrect'=rnorm(1),
       'sigma_CP'=runif(1),
       'sigma_CQ'=runif(1),
       'b_Cparticipant'=rnorm(NParticipants),
       'b_Cquestion'=rnorm(NQuestions),
       'b_Swintercept'=rnorm(1),
       'b_SwSex' = rnorm(1),
       'sigma_SwP' =runif(1),
       'sigma_SwQ' =runif(1),
       'b_SwParticipant'=rnorm(NParticipants),
       'b_SwQuestion' =rnorm(NQuestions),
       'b_SwNoDisagreed' = rnorm(1),
       'b_SwConf' = rnorm(1))
}

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
                          model.file='mediation_Model.jags',
                          n.chains = n.chains,n.iter=61000,n.burnin=1000,
                          n.thin=20,n.cluster=n.cluster)

# This function returns coda compatible objects, for use with coda functions
my.samples <- as.mcmc(my.model)

# Lets focus on the relevant parameters (ignore participant and question pars)
relPars = c('b_Cintercept','b_CCond','b_CSex','b_Ccorrect','sigma_CP_abs','sigma_CQ_abs','b_Swintercept','b_SwSex','sigma_SwP_abs','sigma_SwQ_abs','b_SwNoDisagreed','b_SwConf','theta')

# View the parameter densities and trace
# Save plots
pdf('b_mediation.pdf',width = 8.27,height=11.02)
plot(my.samples[,relPars])
dev.off()

# Save summary statistics
my.summary = summary(my.samples[,relPars])
sink('b_mediation.txt')
my.summary
sink()

# Save samples
my.samples.mediation= my.samples
save('my.samples.mediation',file='my.samples.mediation.dump')


# Odds ratio for sex condition
# the increase in probability of being
# fully confident given that you are male?
# Not sure about this
#exp(my.summary$statistics[2,1])

#maximum loglikelihood
#max(as.matrix(my.samples[,'logLike']))
