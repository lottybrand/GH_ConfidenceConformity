# We need rjags to reed and compile the jags model
# coda gives nice functions for handling chain summaries
# and plots
library(rjags)
library(coda)
library(R2jags)

# Read in the data and construct the jags objects
#source('../Data/loadData_latest.R')






modelNumber = 1
n.chains = 3
n.cluster = n.chains


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


  ##### Logistic regression #####
  if(modelNumber==1){
    #### 1. Full SWITCHING model ####
  
    my.model <- jags.parallel(data=jags.data,inits=jags.init,
                            parameters.to.save = jags.params,
                            model.file='switching_Model.jags',
                            n.chains = n.chains,n.iter=61000,n.burnin=1000,
                            n.thin=20,n.cluster=n.cluster)
  
  # This function returns coda compatible objects, for use with coda functions
  my.samples <- as.mcmc(my.model)
  
  # Lets focus on the relevant parameters (ignore participant and question pars)
  relPars = c('b_sex','b_confidence','b_sex_confidence','b_correct','b_noDisagreed','sigma_p_abs','sigma_q_abs')
  
  # Save plots
  pdf('latest_fullSwitching.pdf',width = 8.27,height=11.02)
  plot(my.samples[,relPars])
  #dev.off()
  
  # Save summary statistics
  my.summary = summary(my.samples[,relPars])
  sink('bgSocial_fullSwitching.txt')
  my.summary
  sink()
  
  # Save samples
  my.samples.fullSwitching = my.samples
  save('my.samples.fullSwitching',file='my.samples.fullSwitching.dump')
  
  }else {
  print("No such model")
}


