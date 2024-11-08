# ouput from ifr_chisq_assoc_test is as expected

    Code
      ifr_chisq_assoc_test(hsb, female, schtyp)
    Output
                     Chi Square Statistics                 
      
      Statistics                     DF    Value      Prob 
      ----------------------------------------------------
      Chi-Square                     1    0.0470    0.8284
      Likelihood Ratio Chi-Square    1    0.0471    0.8282
      Continuity Adj. Chi-Square     1    0.0005    0.9822
      Mantel-Haenszel Chi-Square     1    0.0468    0.8287
      Phi Coefficient                     0.0153          
      Contingency Coefficient             0.0153          
      Cramer's V                          0.0153          
      ----------------------------------------------------

# ouput from ifr_chisq_assoc_test 2 is as expected

    Code
      ifr_chisq_assoc_test(hsb, female, ses)
    Output
                     Chi Square Statistics                 
      
      Statistics                     DF    Value      Prob 
      ----------------------------------------------------
      Chi-Square                     2    4.5765    0.1014
      Likelihood Ratio Chi-Square    2    4.6789    0.0964
      Phi Coefficient                     0.1513          
      Contingency Coefficient             0.1496          
      Cramer's V                          0.1513          
      ----------------------------------------------------

