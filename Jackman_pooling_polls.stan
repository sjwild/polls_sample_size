 // Pooling the polls model based off Simon Jackman's model in his text
 // Bayesian Analysis for the Social Sciences
 
 data {

  // Ns
  int<lower=1> N_days;                     // number of days
  int<lower=1> N_polls;                    // number of polls
  int<lower=1> N_pollsters;                // number of pollsters
  
  // election results
  real start_election;                     // starting election
  real end_election;                       // ending election

  
  // poll results
  array[N_polls] real y;                        // actual values in polls
  array[N_polls] real y_moe;                    // margins of errors based on srs
  
  // poll date + polster id
  array[N_polls] int<lower=1> poll_date;                           // the number of days since first poll was taken
  array[N_polls] int<lower=1,upper=N_pollsters> pollster_id;       // id for each pollster
  
  // sample size for ratio
  array[N_polls] real sample_size_reported;
  
  // nu for DOF in student_t distribution if used
  //real<lower=3> nu;
  
  
  
}


parameters {
  
  vector[N_days-1] z_omega;                           // Matrix to hold std_normal errors for non-centered parameterization 
  real<lower=0> omega;                                // innovation sd
  vector[N_pollsters] log_sigma_pollster;             // to account for error per pollster other than sampling
  real<lower=0> sigma_other;                          // to account for other sources of error
  real mu_log_sigma;                                  // mean of log sigmas. To allow for random effect
  real<lower=0> tau;                                  // sd of log sigmas

  //real<lower=0> sigma_delta;                       // RE sigma for house effects
  //real<lower=0> sigma_gamma;                       // RE sigma for mode effects

  array[N_pollsters] real delta;                     // house effect for each pollster

}

transformed parameters {
  vector<lower=0>[N_pollsters] sigma_pollster;       // to account for error per pollster other than sampling
  vector[N_days-1] Omega;                                    // innovation sd per day
  vector[N_days] xi;                                         // latent vote intention
  vector<lower=0>[N_polls] sigma;                            // total sd per poll
  vector[N_polls] mu;                                        // xi + delta + gamma


  Omega = z_omega * omega;      
  
  sigma_pollster = exp(log_sigma_pollster);

  // set up non-centered paramterization for innovation sd
  xi[1] = start_election;

  for(t in 2:(N_days)){
    xi[t] = xi[t-1] + Omega[t-1];
  }

  // build mu and sigma for likelihood
  for(i in 1:N_polls){ 
    mu[i] = xi[poll_date[i]] + delta[pollster_id[i]];
      
    sigma[i] = sqrt(square(sigma_pollster[pollster_id[i]]) +
        square(y_moe[i]) + square(sigma_other));


  }
    

}


model {
  
  // priors for innovation sd 
  target += normal_lpdf(z_omega | 0, 1);
  target += exponential_lpdf(omega | 5);
  
  // priors on xi N_days, to use anchors
  target += normal_lpdf(end_election | xi[N_days-1], 0.001);

  // priors for various sigmas
  target += exponential_lpdf(tau | 5);
  target += normal_lpdf(mu_log_sigma | -4, 0.5);
  target += normal_lpdf(log_sigma_pollster | mu_log_sigma, tau);
  target += exponential_lpdf(sigma_other | 5);

 
  // prior for house effects and survey mode effects 
  target += normal_lpdf(delta | 0, 0.05);                    // house effects
  
   // Likelihood
   target += normal_lpdf(y | mu, sigma);

  
}


generated quantities {
  array[N_polls] real sample_size;
  array[N_polls] real sample_ratio;
  
  for(n in 1:N_polls){
    sample_size[n] = (y[n] * (1 - y[n])) / square(sigma[n]);
    sample_ratio[n] = sample_size[n] / sample_size_reported[n];
  }

}


