# We need rjags to reed and compile the jags model
# coda gives nice functions for handling chain summaries
# and plots
library(rjags)
library(coda)
library(R2jags)

# Read in the data and construct the jags objects
source('../loadData_pilotData.R')


modelNumber = 2
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
                              model.file='confidence_Model_full.jags',
                              n.chains = n.chains,n.iter=61000,n.burnin=1000,
                              n.thin=20,n.cluster=n.cluster)
    # Running time
    proc.time() - ptm
    
    # This function returns coda compatible objects, for use with coda functions
    my.samples <- as.mcmc(my.model)
    
    # Lets focus on the relevant parameters (ignore participant and question pars)
    relPars = c('b_intercept','b_sex','b_cond1','b_cond2','b_sex_cond1','b_sex_cond2','b_correct','sigma_p_abs','sigma_q_abs')
    
    # Save plots
    pdf('tracePlots_confidenceFull.pdf',width = 8.27,height=11.02)
    plot(my.samples[,relPars])
    dev.off()
    
    # Save summary statistics
    my.summary = summary(my.samples[,relPars])
    sink('confidenceFull_results.txt')
    my.summary
    sink()
    
    # Save samples
    my.samples.confidenceFull = my.samples
    save('my.samples.confidenceFull',file='my.samples.confidenceFull.dump')
    
  }else if(modelNumber==2){
    #### 2. Interested in sex diff across all conditions ####
    
    ptm = proc.time()
    my.model <- jags.parallel(data=jags.data,inits=jags.init,
                              parameters.to.save = jags.params,
                              model.file='confidence_model_Sex.jags',
                              n.chains = n.chains,n.iter=61000,n.burnin=1000,
                              n.thin=20,n.cluster=n.cluster)
    # Running time
    proc.time() - ptm
    
    # This function returns coda compatible objects, for use with coda functions
    my.samples <- as.mcmc(my.model)
    
    # Lets focus on the relevant parameters (ignore participant and question pars)
    relPars = c('b_intercept','b_sex','b_correct','sigma_p_abs','sigma_q_abs')
    
    # Save plots
    pdf('pilot_tracePlots_confidenceSex.pdf',width = 8.27,height=11.02)
    plot(my.samples[,relPars])
    dev.off()
    
    # Save summary statistics
    my.summary = summary(my.samples[,relPars])
    sink('pilot_confidenceSex_results.txt')
    my.summary
    sink()
    
    # Save samples
    my.samples.confidenceSex = my.samples
    save('my.samples.confidenceSex',file='my.samples.confidenceSex.dump')
    

  }else {
    print("No such model")
  }
