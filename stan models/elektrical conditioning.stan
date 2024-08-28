data {
  int<lower=0> N;
  vector[N] percepts;
  vector[N] expectations;
  vector[N] stim;
  array[N%/%2] int low_cue_id;
  array[N%/%2] int high_cue_id;
  
  
}

parameters {
  real<lower=0, upper = 1> alpha_c;
  real<lower=0, upper = 1> alpha_dc;

  real<lower=0, upper = 1> gamma;
  
  real<lower=0> prec_pain;
  real<lower=0> prec_expectation;
  
  real<lower=0, upper = 1> e0_low;
  real<lower=0, upper = 1> e0_high;
  
  real<lower=0, upper = 1> stim_low;
  real<lower=0, upper = 1> stim_high;
  
  
  
}
transformed parameters{
  vector <lower=0, upper =1> [N%/%2] mu_expectations_high;
  vector <lower=0, upper =1> [N%/%2] mu_expectations_low;

  vector <lower=0, upper =1> [N%/%2] mu_pain_high;
  vector <lower=0, upper =1> [N%/%2] mu_pain_low;

  vector <lower=-1, upper =1> [N%/%2] pe_high;
  vector <lower=-1, upper =1> [N%/%2] pe_low; 
  
  vector <lower=0, upper =1> [N] mu_expectations;
  vector <lower=0, upper =1> [N] mu_pain;
  
  
  mu_expectations_low[1] = e0_low;
  mu_expectations_high[1] = e0_high;
  
  for(low in 1:(size(low_cue_id))){
     if(low != size(low_cue_id)){
       
      mu_expectations[low_cue_id[low]] = mu_expectations_low[low];
      
      if(stim[low_cue_id[low]] > 0.5){
        mu_pain_low[low] = (1-gamma)*stim_high+gamma*mu_expectations_low[low];
      }else{
        mu_pain_low[low] = (1-gamma)*stim_low+gamma*mu_expectations_low[low];
      }
      
      mu_pain[low_cue_id[low]] = mu_pain_low[low];
      
      pe_low[low] = mu_pain_low[low]-mu_expectations_low[low];
      
    if(pe_low[low] < 0){
      mu_expectations_low[low+1] = mu_expectations_low[low]+alpha_c*pe_low[low];
    }else{
      mu_expectations_low[low+1] = mu_expectations_low[low]+alpha_dc*pe_low[low];
    }
    //last trial of low_cues
    }else{
      
      mu_expectations[low_cue_id[low]] = mu_expectations_low[low];
      
      if(stim[low_cue_id[low]] > 0.5){
        mu_pain_low[low] = (1-gamma)*stim_high+gamma*mu_expectations_low[low];
      }else{
        mu_pain_low[low] = (1-gamma)*stim_low+gamma*mu_expectations_low[low];
      }
 
 
      mu_pain[low_cue_id[low]] = mu_pain_low[low];
      pe_low[low] = mu_pain_low[low]-mu_expectations_low[low];
      
    }
  }
  
  for(high in 1:(size(high_cue_id))){
     if(high != size(high_cue_id)){
       
      mu_expectations[high_cue_id[high]] = mu_expectations_high[high];
       

      if(stim[high_cue_id[high]] > 0.5){
        mu_pain_high[high] = (1-gamma)*stim_high+gamma*mu_expectations_high[high];
      }else{
        mu_pain_high[high] = (1-gamma)*stim_low+gamma*mu_expectations_high[high];
      }
      
      
      mu_pain[high_cue_id[high]] = mu_pain_high[high];
    
      pe_high[high] = mu_pain_high[high]-mu_expectations_high[high];
      
    if(pe_high[high] < 0){
      mu_expectations_high[high+1] = mu_expectations_high[high]+alpha_dc*pe_high[high];
    }else{
      mu_expectations_high[high+1] = mu_expectations_high[high]+alpha_c*pe_high[high];
    }
     
    //last trial
    }else{
      
      mu_expectations[high_cue_id[high]] = mu_expectations_high[high];

      if(stim[high_cue_id[high]] > 0.5){
        mu_pain_high[high] = (1-gamma)*stim_high+gamma*mu_expectations_high[high];
      }else{
        mu_pain_high[high] = (1-gamma)*stim_low+gamma*mu_expectations_high[high];
      }

      
      mu_pain[high_cue_id[high]] = mu_pain_high[high];

      pe_high[high] = mu_pain_high[high]-mu_expectations_high[high];
      
    }
  }


}

model {
  target += beta_proportion_lpdf(alpha_c | 0.3, 5);
  target += beta_proportion_lpdf(alpha_dc | 0.3, 5);
  
  target += beta_proportion_lpdf(gamma | 0.3, 5);
  
  
  target += beta_proportion_lpdf(e0_low | 0.3, 5);
  target += beta_proportion_lpdf(e0_high | 0.7, 5);
  
  target += beta_proportion_lpdf(stim_low | 0.3, 5);
  target += beta_proportion_lpdf(stim_high | 0.7, 5);
  
  
  target += gamma_lpdf(prec_pain | 1, 0.05);
  target += gamma_lpdf(prec_expectation | 1, 0.05);
  
  
  
  for(t in 1:N){
    target += beta_proportion_lpdf(percepts[t] | mu_pain[t], prec_pain);
    target += beta_proportion_lpdf(expectations[t] | mu_expectations[t], prec_expectation);
  }
}


generated quantities{
   vector[N] pred_expect;
   vector[N] pred_percept;

  real <lower=0, upper =1> prior_alpha_c;
  real <lower=0, upper =1> prior_alpha_dc;
  
  real <lower=0, upper =1> prior_gamma;
  real <lower=0, upper =1> prior_e0_low;
  real <lower=0, upper =1> prior_e0_high;
  real <lower=0, upper =1> prior_stim_low;
  real <lower=0, upper =1> prior_stim_high;
  
  
  real <lower=0> prior_prec_pain;
  real <lower=0> prior_prec_expectation;
  
  prior_alpha_c = beta_proportion_rng(0.3,5);
  prior_alpha_dc = beta_proportion_rng(0.3,5);
  prior_gamma = beta_proportion_rng(0.3,5);
  
  prior_e0_low = beta_proportion_rng(0.3,5);
  prior_e0_high = beta_proportion_rng(0.7,5);
  
  prior_stim_low = beta_proportion_rng(0.3,5);
  prior_stim_high = beta_proportion_rng(0.7,5);
  
  prior_prec_pain = gamma_rng(1,0.05);
  prior_prec_expectation = gamma_rng(1,0.05);  
  
  for(i in 1:N){
    pred_percept[i] = beta_proportion_rng(mu_pain[i], prec_pain);
    pred_expect[i] = beta_proportion_rng(mu_expectations[i], prec_expectation);
}
  
}
