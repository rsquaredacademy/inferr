# output from 2 sample proportion test is as expected when alternative is less

    Code
      ifr_ts_prop_group(treatment2, outcome, female, alternative = "less")
    Output
             Test Statistics        
      -----------------------------
      Total Observations        200 
      z                       0.351 
      Pr(Z < z)               0.637 
      

# output from 2 sample proportion test is as expected when alternative is greater

    Code
      ifr_ts_prop_group(treatment2, outcome, female, alternative = "greater")
    Output
             Test Statistics        
      -----------------------------
      Total Observations        200 
      z                       0.351 
      Pr(Z > z)               0.363 
      

# output from 2 sample proportion test is as expected when alternative is both

    Code
      ifr_ts_prop_group(treatment2, outcome, female, alternative = "both")
    Output
             Test Statistics        
      -----------------------------
      Total Observations        200 
      z                       0.351 
      Pr(|Z| < |z|)           0.726 
      

# output from 2 sample proportion test is as expected when alternative is all

    Code
      ifr_ts_prop_group(treatment2, outcome, female, alternative = "all")
    Output
             Test Statistics        
      -----------------------------
      Total Observations        200 
      z                       0.351 
      Pr(|Z| < |z|)           0.726 
      Pr(Z < z)               0.637 
      Pr(Z > z)               0.363 
      

