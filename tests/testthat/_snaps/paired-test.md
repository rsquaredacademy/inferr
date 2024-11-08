# output from paired sample t test is as expected when alternative is less

    Code
      ifr_ts_paired_ttest(hsb, read, write, alternative = "less")
    Output
                               Paired Samples Statistics                           
      ----------------------------------------------------------------------------
      Variables    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
      ----------------------------------------------------------------------------
         read       200    52.23     0.7249921    10.25294        50.8      53.66   
         write      200    52.775    0.6702372    9.478586       51.45       54.1    
      ----------------------------------------------------------------------------
         diff       200    -0.545    0.6283822    8.886666       -1.78       0.69    
      ----------------------------------------------------------------------------
      
               Paired Samples Correlations         
      -------------------------------------------
        Variables      Obs    Correlation    Sig.
       read & write    200       0.60        0 
      -------------------------------------------
      
                Paired Samples Test            
                -------------------            
             Ho: mean(read - write) = 0        
             Ha: mean(read - write) < 0        
      
      ----------------------------------------
        Variables         t       df     Sig.  
      ----------------------------------------
       read - write    -0.8673    199    0.193 
      ----------------------------------------

# output from paired sample t test is as expected when alternative is greater

    Code
      ifr_ts_paired_ttest(hsb, read, write, alternative = "greater")
    Output
                               Paired Samples Statistics                           
      ----------------------------------------------------------------------------
      Variables    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
      ----------------------------------------------------------------------------
         read       200    52.23     0.7249921    10.25294        50.8      53.66   
         write      200    52.775    0.6702372    9.478586       51.45       54.1    
      ----------------------------------------------------------------------------
         diff       200    -0.545    0.6283822    8.886666       -1.78       0.69    
      ----------------------------------------------------------------------------
      
               Paired Samples Correlations         
      -------------------------------------------
        Variables      Obs    Correlation    Sig.
       read & write    200       0.60        0 
      -------------------------------------------
      
                Paired Samples Test            
                -------------------            
             Ho: mean(read - write) = 0        
             Ha: mean(read - write) > 0        
      
      ----------------------------------------
        Variables         t       df     Sig.  
      ----------------------------------------
       read - write    -0.8673    199    0.807 
      ----------------------------------------

# output from paired sample t test is as expected when alternative is both

    Code
      ifr_ts_paired_ttest(hsb, read, write, alternative = "both")
    Output
                               Paired Samples Statistics                           
      ----------------------------------------------------------------------------
      Variables    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
      ----------------------------------------------------------------------------
         read       200    52.23     0.7249921    10.25294        50.8      53.66   
         write      200    52.775    0.6702372    9.478586       51.45       54.1    
      ----------------------------------------------------------------------------
         diff       200    -0.545    0.6283822    8.886666       -1.78       0.69    
      ----------------------------------------------------------------------------
      
               Paired Samples Correlations         
      -------------------------------------------
        Variables      Obs    Correlation    Sig.
       read & write    200       0.60        0 
      -------------------------------------------
      
                Paired Samples Test            
                -------------------            
             Ho: mean(read - write) = 0        
            Ha: mean(read - write) ~= 0        
      
      ----------------------------------------
        Variables         t       df     Sig.  
      ----------------------------------------
       read - write    -0.8673    199    0.387 
      ----------------------------------------

# output from paired sample t test is as expected when alternative is all

    Code
      ifr_ts_paired_ttest(hsb, read, write, alternative = "all")
    Output
                               Paired Samples Statistics                           
      ----------------------------------------------------------------------------
      Variables    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
      ----------------------------------------------------------------------------
         read       200    52.23     0.7249921    10.25294        50.8      53.66   
         write      200    52.775    0.6702372    9.478586       51.45       54.1    
      ----------------------------------------------------------------------------
         diff       200    -0.545    0.6283822    8.886666       -1.78       0.69    
      ----------------------------------------------------------------------------
      
               Paired Samples Correlations         
      -------------------------------------------
        Variables      Obs    Correlation    Sig.
       read & write    200       0.60        0 
      -------------------------------------------
      
                      Ho: mean(read - write) = mean(diff) = 0                  
      
         Ha: mean(diff) < 0      Ha: mean(diff) ~= 0       Ha: mean(diff) > 0    
             t = -0.8673              t = -0.8673              t = -0.8673       
           P < t = 0.193           P > |t| = 0.387           P > t = 0.807       

