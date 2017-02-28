# Jags model definition, data values here
# must match those in the data declaration

# Model is defined as follows
model{
  # Some notes from the jags manual
  # Nodes
  #   Each relation defines a node in the model in terms of other nodes that appear on the right
  #   hand side. These are referred to as the parent nodes. Taken together, the nodes in the model
  #   (together with the parent/child relationships represented as directed edges) form a directed
  #   acyclic graph. The very top-level nodes in the graph, with no parents, are constant nodes,
  #   which are defined either in the model definition (e.g. 1.0E-3), or in the data file ( e.g.x[1])

  #   Relations can be of two types. A stochastic relation ( ~ ) defines a stochastic node, repre
  #   senting a random
  #   variable in the model. A deterministic relation ( <- ) defines a
  #   deterministic node, the value of which is determined exactly by the values
  #   of its parents
  
  #   Deterministic nodes do not need to be embedded in node arrays.  

  # Order does not matter here, but lets do it sensibly?  
  
 
  # Priors  
  # Use an uninformative prior, normal distribution
  # with mean 0 and sd 10000
  # sd = 1/(0.01)^2 = 10000 ~ not very informative
  b_sex ~ dnorm(0,0.01)
  b_cond1 ~ dnorm(0,0.01)
  b_cond2 ~ dnorm(0,0.01)
  b_sex_cond1 ~ dnorm(0,0.01)
  b_sex_cond2 ~ dnorm(0,0.01)
  b_correct ~ dnorm(0,0.01)
  b_age ~ dnorm(0,0.01)
  b_education ~ dnorm(0,0.01)
  b_stimulus ~ dnorm(0,0.01)
  b_sex_stimulus ~ dnorm(0,0.01)
  
  # Use the half cauchy prior for the variance parameters  
  # of the random effects
  # THe participant random effect
  # Cauchy is dt with df = 1
  sigma_p ~ dt(0,1,1)
  # Get the half cauchy
  sigma_p_abs <- abs(sigma_p)
  # Convert to precision for norm dist
  tau_p <- pow(sigma_p, -2)  
  
  # Repeat for the question random effect
  sigma_q ~ dt(0,1,1) 
  sigma_q_abs <- abs(sigma_q)
  tau_q <- pow(sigma_q_abs, -2)
  
  # Generate a prior for the mean effect for each 
  # participant using this variance this
  for(i in 1:NParticipants){
    b_participant[i] ~ dnorm(0,tau_p)
  }
  # The same for the question
  for(i in 1:NQuestions){
    b_question[i] ~ dnorm(0,tau_q)
  } 

  # The priors over thresholds or 'cutoffs' for the ordered logit distribution
  for(i in 1:6){
    alpha0[i] ~ dnorm(0,0.01)
  }
  # They need to be in ascending order
  alpha <- sort(alpha0)

  # The model 
  # Iterate over every data point we have
  for(i in 1:NObservations){
            
      # A deterministic node      
      # Calculate the regressino values
      z[i] <- b_sex*Sex[i] + b_cond1*Condition1[i] + b_cond2*Condition2[i] + 
        b_sex_cond1*Sex[i]*Condition1[i] + b_sex_cond2*Sex[i]*Condition2[i] + 
        b_correct*Correct[i] + b_education*Education[i] + b_age*Age[i] + 
        b_stimulus*Stimulus[i] + b_sex_stimulus*Sex[i]*Stimulus[i] + 
        b_participant[ParticipantID[i]] + b_question[QuestionID[i]]

      # The logistic link function - note we need to use the inverse with jags
      # Here we incorporate the 'cutoff' values alpha,
      # We want the cumulative log odds of getting a value equal to or
      # lower than this cutoff alpha
      Q[i,1] <- ilogit(alpha[1] - z[i])   
      P[i,1] <- Q[i,1]

      for(j in 2:6){
        Q[i,j] <- ilogit(alpha[j] - z[i])
        P[i,j] <- Q[i,j] - Q[i,j-1]
      }
     
      P[i,7] <- 1 - Q[i,6]

      # A stochastic node 
      Confidence2[i] ~ dcat(P[i,])
  }
}
