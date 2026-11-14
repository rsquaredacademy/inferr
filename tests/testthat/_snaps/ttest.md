# output from one sample t test is as expected when alternative is less

    Code
      ifr_os_t_test(hsb, write, mu = 50, alternative = "less")
    Output
                                    One-Sample Statistics                               
      ---------------------------------------------------------------------------------
       Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
      ---------------------------------------------------------------------------------
        write      200    52.775     0.6702       9.4786         -Inf     53.8826   
      ---------------------------------------------------------------------------------
      
                                       Lower Tail Test                                
                                       ---------------                                 
      
                                    Ho: mean(write) >=50                              
                                     Ha: mean(write) <50                               
      -------------------------------------------------------------------------------
       Variable     t      DF       Sig       Mean Diff.    [95% Conf. Interval] 
      -------------------------------------------------------------------------------
        write      4.14    199    0.99997       2.775          -Inf      3.8826   
      -------------------------------------------------------------------------------

# output from one sample t test is as expected when alternative is greater

    Code
      ifr_os_t_test(hsb, write, mu = 50, alternative = "greater")
    Output
                                    One-Sample Statistics                               
      ---------------------------------------------------------------------------------
       Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
      ---------------------------------------------------------------------------------
        write      200    52.775     0.6702       9.4786       51.6674      Inf     
      ---------------------------------------------------------------------------------
      
                                       Upper Tail Test                                
                                       ---------------                                 
      
                                    Ho: mean(write) <=50                              
                                     Ha: mean(write) >50                               
      -------------------------------------------------------------------------------
       Variable     t      DF       Sig       Mean Diff.    [95% Conf. Interval] 
      -------------------------------------------------------------------------------
        write      4.14    199    0.00003       2.775         1.6674      Inf     
      -------------------------------------------------------------------------------

# output from one sample t test is as expected when alternative is both

    Code
      ifr_os_t_test(hsb, write, mu = 50, alternative = "both")
    Output
                                    One-Sample Statistics                               
      ---------------------------------------------------------------------------------
       Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
      ---------------------------------------------------------------------------------
        write      200    52.775     0.6702       9.4786       51.4533    54.0967   
      ---------------------------------------------------------------------------------
      
                                        Two Tail Test                                 
                                       ---------------                                 
      
                                    Ho: mean(write) ~=50                              
                                    Ha: mean(write) !=50                               
      -------------------------------------------------------------------------------
       Variable     t      DF       Sig       Mean Diff.    [95% Conf. Interval] 
      -------------------------------------------------------------------------------
        write      4.14    199    0.00005       2.775         1.4533     4.0967   
      -------------------------------------------------------------------------------

# output from one sample t test is as expected when alternative is all

    Code
      ifr_os_t_test(hsb, write, mu = 50, alternative = "all")
    Output
                                    One-Sample Statistics                               
      ---------------------------------------------------------------------------------
       Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
      ---------------------------------------------------------------------------------
        write      200    52.775     0.6702       9.4786       51.4533    54.0967   
      ---------------------------------------------------------------------------------
      
                                    Ho: mean(write) ~=50                              
      
              Ha: mean < 50              Ha: mean ~= 50               Ha: mean > 50        
                t = 4.14                    t = 4.14                    t = 4.14         
             P < t = 0.99997            P > |t| = 0.00005            P > t = 0.00003      

