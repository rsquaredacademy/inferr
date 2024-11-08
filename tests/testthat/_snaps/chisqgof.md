# output from ifr_chisq_gof_test is as expected

    Code
      ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140))
    Output
          Test Statistics     
      -----------------------
      Chi-Square       5.0286 
      DF                    3 
      Pr > Chi Sq      0.1697 
      Sample Size         200 
      
                               Variable: race                           
      -----------------------------------------------------------------
      Category    Observed    Expected    % Deviation    Std. Residuals 
      -----------------------------------------------------------------
         1           24          20          20.00            0.89      
         2           11          20         -45.00           -2.01      
         3           20          20           0.00            0.00      
         4          145         140           3.57            0.42      
      -----------------------------------------------------------------

