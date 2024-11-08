# output from mcnemar test is as expected

    Code
      ifr_mcnemar_test(table(himath, hiread))
    Output
                 Controls 
      ---------------------------------
      Cases       0       1       Total 
      ---------------------------------
        0        135      21        156 
        1         18      26         44 
      ---------------------------------
      Total      153      47        200 
      ---------------------------------
      
             McNemar's Test        
      ----------------------------
      McNemar's chi2        0.2308 
      DF                         1 
      Pr > chi2              0.631 
      Exact Pr >= chi2      0.7493 
      ----------------------------
      
             Kappa Coefficient         
      --------------------------------
      Kappa                     0.4454 
      ASE                        0.075 
      95% Lower Conf Limit      0.2984 
      95% Upper Conf Limit      0.5923 
      --------------------------------
      
      Proportion With Factor 
      ----------------------
      cases             0.78 
      controls         0.765 
      ratio           1.0196 
      odds ratio      1.1667 
      ----------------------

