# We need rjags to reed and compile the jags model
# coda gives nice functions for handling chain summaries
# and plots
library(rjags)
library(coda)
library(R2jags)

# Read in the data and construct the jags objects
source('../Data/loadData_latest.R')


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
  #### 1. A priori ####
  
  ptm = proc.time()
  my.model <- jags.parallel(data=jags.data,inits=jags.init,
                            parameters.to.save = jags.params,
                            model.file='performance_Model_full.jags',
                            n.chains = n.chains,n.iter=61000,n.burnin=1000,
                            n.thin=20,n.cluster=n.cluster)
  # Running time
  proc.time() - ptm
  
  # This function returns coda compatible objects, for use with coda functions
  my.samples <- as.mcmc(my.model)
  
  # Lets focus on the relevant parameters (ignore participant and question pars)
  relPars = c('b_intercept','b_sex','b_cond1','b_cond2','b_sex_cond1','b_sex_cond2','sigma_p_abs','sigma_q_abs')
  
  # Save plots
  #pdf('tracePlots_performance_full.pdf',width = 8.27,height=11.02)
  #plot(my.samples[,relPars])
  #dev.off()
  
  # Save summary statistics
  my.summary = summary(my.samples[,relPars])
  sink('performance_full_results.txt')
  my.summary
  sink()
  
  # Save samples
  my.samples.performance_full = my.samples
  save('my.samples.performance_full',file='my.samples.performance_full.dump')
  
}else if(modelNumber==2){
  #### 2. interested in general sex diff across all conditions ####
  
  ptm = proc.time()
  my.model <- jags.parallel(data=jags.data,inits=jags.init,
                            parameters.to.save = jags.params,
                            model.file='performance_Model_Sex.jags',
                            n.chains = n.chains,n.iter=61000,n.burnin=1000,
                            n.thin=20,n.cluster=n.cluster)
  # Running time
  proc.time() - ptm
  
  # This function returns coda compatible objects, for use with coda functions
  my.samples <- as.mcmc(my.model)
  
  # Lets focus on the relevant parameters (ignore participant and question pars)
  relPars = c('b_intercept','b_sex','sigma_p_abs','sigma_q_abs')
  
  # Save plots
  pdf('tracePlots_performanceSex.pdf',width = 8.27,height=11.02)
  plot(my.samples[,relPars])
  dev.off()
  
  # Save summary statistics
  my.summary = summary(my.samples[,relPars])
  sink('performanceSex_results.txt')
  my.summary
  sink()
  
  # Save samples
  my.samples.performance_sex = my.samples
  save('my.samples.performance_sex',file='my.samples.performance_sex.dump')

}else {
  print("No such model")
}
